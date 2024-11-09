use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{multispace0, multispace1},
    multi::{many0, separated_list0},
    sequence::{delimited, preceded, terminated},
    IResult,
};
use nom_locate::LocatedSpan;

use crate::{
    break_result::{BreakResult, EvalResult},
    expression::{self, Expression, Ident},
    function::{FnDef, UserFn},
    helper,
    stack_frame::StackFrame,
    type_check::{self, Span, TypeCheckContext, TypeCheckError, TypeDeclare},
    value::Value,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<'src> {
    VarDef {
        span: Span<'src>,
        name: Ident<'src>,
        type_declare: TypeDeclare,
        expression: Expression<'src>,
    },
    Assignment {
        span: Span<'src>,
        name: Ident<'src>,
        expression: Expression<'src>,
    },
    Expression {
        span: Span<'src>,
        expression: Expression<'src>,
    },
    For {
        span: Span<'src>,
        loop_var: Ident<'src>,
        start: Expression<'src>,
        end: Expression<'src>,
        body: Statements<'src>,
    },
    FnDef {
        span: Span<'src>,
        name: Ident<'src>,
        params: Vec<(Ident<'src>, TypeDeclare)>,
        return_type: TypeDeclare,
        body: Statements<'src>,
    },
    Return(Expression<'src>),
    Break,
    Continue,
}

impl<'src> Statement<'src> {
    pub fn parse(
        input: LocatedSpan<&'src str>,
    ) -> IResult<LocatedSpan<&'src str>, Statement<'src>> {
        let (input, statement) = alt((
            for_statement,
            fn_def,
            terminated(return_statement, terminator),
            terminated(var_def, terminator),
            terminated(assignment, terminator),
            terminated(break_statement, terminator),
            terminated(continue_statement, terminator),
            terminated(expr_statement, terminator),
        ))(input)?;
        Ok((input, statement))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Statements<'src>(Vec<Statement<'src>>);

impl<'src> Statements<'src> {
    pub fn parse(
        input: LocatedSpan<&'src str>,
    ) -> IResult<LocatedSpan<&'src str>, Statements<'src>> {
        let (input, statements) = many0(Statement::parse)(input)?;
        Ok((input, Statements(statements)))
    }

    pub fn eval(&'src self, stack_frame: &mut StackFrame<'src>) -> EvalResult {
        let mut result = EvalResult::Continue(Value::I64(0));
        for statement in self.0.iter() {
            match statement {
                Statement::VarDef {
                    span: _,
                    name,
                    type_declare: _,
                    expression,
                } => {
                    let value = expression.eval(stack_frame)?;
                    stack_frame.insert_variable(*name, value);
                }
                Statement::Assignment {
                    span,
                    name,
                    expression,
                } => {
                    if stack_frame.get_variable(*name).is_none() {
                        panic!("{}\nError: {:?} is not defined", span, name);
                    }
                    let value = expression.eval(stack_frame)?;
                    stack_frame.insert_variable(*name, value);
                }
                Statement::Expression {
                    span: _,
                    expression,
                } => {
                    result = EvalResult::Continue(expression.eval(stack_frame)?);
                }
                Statement::For {
                    span: _,
                    loop_var,
                    start,
                    end,
                    body,
                } => {
                    let start = start.eval(stack_frame)?.coerce_to_i64();
                    let end = end.eval(stack_frame)?.coerce_to_i64();
                    for i in start..end {
                        stack_frame.insert_variable(*loop_var, Value::I64(i));
                        match body.eval(stack_frame) {
                            EvalResult::Continue(n) => {
                                result = EvalResult::Continue(n);
                            }
                            EvalResult::Break(BreakResult::Break) => {
                                break;
                            }
                            EvalResult::Break(BreakResult::Continue) => {
                                continue;
                            }
                            EvalResult::Break(BreakResult::Return(n)) => {
                                return EvalResult::Break(BreakResult::Return(n));
                            }
                        };
                    }
                }
                Statement::FnDef {
                    span: _,
                    name,
                    params,
                    return_type,
                    body,
                } => {
                    let fn_def = FnDef::User(UserFn::new(&params[..], *return_type, &body));
                    stack_frame.insert_function(*name, fn_def);
                }
                Statement::Return(expression) => {
                    return EvalResult::Break(BreakResult::Return(expression.eval(stack_frame)?));
                }
                Statement::Break => {
                    return EvalResult::Break(BreakResult::Break);
                }
                Statement::Continue => {
                    return EvalResult::Break(BreakResult::Continue);
                }
            }
        }
        result
    }

    pub fn type_check(
        &'src self,
        context: &mut TypeCheckContext<'src, '_>,
    ) -> Result<TypeDeclare, TypeCheckError<'src>> {
        let mut result = TypeDeclare::EmptyTuple;
        for statement in self.0.iter() {
            result = match statement {
                Statement::VarDef {
                    span,
                    name,
                    type_declare,
                    expression,
                } => {
                    let expr_type = expression.type_check(context)?;
                    type_declare.coerce_type(&expr_type, *span)?;
                    context.insert_variable(*name, expr_type);
                    TypeDeclare::EmptyTuple
                }
                Statement::Assignment {
                    span,
                    name,
                    expression,
                } => {
                    let expr_type = expression.type_check(context)?;
                    let var_type = context
                        .get_variable(name)
                        .ok_or(TypeCheckError::UndefinedVariable(*span, *name))?;
                    var_type.coerce_type(&expr_type, *span)?;
                    TypeDeclare::EmptyTuple
                }
                Statement::Expression {
                    span: _,
                    expression,
                } => expression.type_check(context)?,
                Statement::For {
                    span,
                    loop_var,
                    start,
                    end,
                    body,
                } => {
                    let start_type = start.type_check(context)?;
                    let end_type = end.type_check(context)?;
                    start_type.coerce_type(&TypeDeclare::I64, *span)?;
                    end_type.coerce_type(&TypeDeclare::I64, *span)?;
                    context.insert_variable(*loop_var, TypeDeclare::I64);
                    body.type_check(context)? // TODO: ループ内変数を削除する、 type_check の返り値を ControlFlow に置き換える
                }
                Statement::FnDef {
                    span,
                    name,
                    params,
                    return_type,
                    body,
                } => {
                    // コンテキストへの関数の代入
                    let fn_def = FnDef::User(UserFn::new(&params[..], *return_type, &body));
                    context.insert_function(*name, fn_def);
                    let mut sub_context = TypeCheckContext::push(context);
                    for (ident, type_declare) in params {
                        sub_context.insert_variable(*ident, *type_declare);
                    }
                    let body_type = body.type_check(&mut sub_context)?;
                    body_type.coerce_type(&return_type, *span)?;
                    TypeDeclare::EmptyTuple
                }
                Statement::Return(expression) => return expression.type_check(context),
                Statement::Break => TypeDeclare::EmptyTuple,
                Statement::Continue => TypeDeclare::EmptyTuple,
            };
        }
        Ok(result)
    }
}

fn terminator<'src>(input: LocatedSpan<&'src str>) -> IResult<LocatedSpan<&'src str>, ()> {
    let (input, _) = delimited(multispace0, tag(";"), multispace0)(input)?;
    Ok((input, ()))
}

fn var_def<'src>(
    input: LocatedSpan<&'src str>,
) -> IResult<LocatedSpan<&'src str>, Statement<'src>> {
    let (rest, _) = delimited(multispace0, tag("let"), multispace1)(input)?;
    let (rest, name) = expression::ident(rest)?;
    let (rest, _) = delimited(multispace0, tag(":"), multispace0)(rest)?;
    let (rest, type_declare) = type_check::TypeDeclare::parse(rest)?;
    let (rest, _) = delimited(multispace0, tag("="), multispace0)(rest)?;
    let (rest, expression) = expression::expr(rest)?;
    Ok((
        rest,
        Statement::VarDef {
            span: helper::span_taken(input, rest),
            name,
            type_declare,
            expression,
        },
    ))
}

fn assignment<'src>(
    input: LocatedSpan<&'src str>,
) -> IResult<LocatedSpan<&'src str>, Statement<'src>> {
    let (rest, name) = expression::ident(input)?;
    let (rest, _) = delimited(multispace0, tag("="), multispace0)(rest)?;
    let (rest, expression) = expression::expr(rest)?;
    Ok((
        rest,
        Statement::Assignment {
            span: helper::span_taken(input, rest),
            name,
            expression,
        },
    ))
}

fn expr_statement<'src>(
    input: LocatedSpan<&'src str>,
) -> IResult<LocatedSpan<&'src str>, Statement<'src>> {
    let (rest, expression) = expression::expr(input)?;
    Ok((
        rest,
        Statement::Expression {
            span: helper::span_taken(input, rest),
            expression,
        },
    ))
}

fn for_statement<'src>(
    input: LocatedSpan<&'src str>,
) -> IResult<LocatedSpan<&'src str>, Statement<'src>> {
    let (rest, _) = delimited(multispace0, tag("for"), multispace1)(input)?;
    let (rest, loop_var) = expression::ident(rest)?;
    let (rest, _) = delimited(multispace0, tag("in"), multispace0)(rest)?;
    let (rest, start) = expression::expr(rest)?;
    let (rest, _) = delimited(multispace0, tag("to"), multispace0)(rest)?;
    let (rest, end) = expression::expr(rest)?;
    let (rest, body) = delimited(helper::open_brace, Statements::parse, helper::close_brace)(rest)?;
    Ok((
        rest,
        Statement::For {
            span: helper::span_taken(input, rest),
            loop_var,
            start,
            end,
            body,
        },
    ))
}

fn fn_def<'src>(input: LocatedSpan<&'src str>) -> IResult<LocatedSpan<&'src str>, Statement<'src>> {
    let (rest, _) = delimited(multispace0, tag("fn"), multispace1)(input)?;
    let (rest, name) = expression::ident(rest)?;
    let (rest, params) = delimited(
        multispace0,
        delimited(tag("("), separated_list0(tag(","), parameter), tag(")")),
        multispace0,
    )(rest)?;
    let (rest, return_type) = preceded(
        delimited(multispace0, tag("->"), multispace0),
        TypeDeclare::parse,
    )(rest)?;
    let (rest, body) = delimited(helper::open_brace, Statements::parse, helper::close_brace)(rest)?;
    Ok((
        rest,
        Statement::FnDef {
            span: helper::span_taken(input, rest),
            name,
            params,
            return_type,
            body,
        },
    ))
}

fn parameter<'src>(
    input: LocatedSpan<&'src str>,
) -> IResult<LocatedSpan<&'src str>, (Ident<'src>, TypeDeclare)> {
    let (input, name) = expression::ident(input)?;
    let (input, _) = delimited(multispace0, tag(":"), multispace0)(input)?;
    let (input, type_declare) = TypeDeclare::parse(input)?;
    Ok((input, (name, type_declare)))
}

fn return_statement<'src>(
    input: LocatedSpan<&'src str>,
) -> IResult<LocatedSpan<&'src str>, Statement<'src>> {
    let (input, _) = delimited(multispace0, tag("return"), multispace1)(input)?;
    let (input, expr) = expression::expr(input)?;
    Ok((input, Statement::Return(expr)))
}

fn break_statement<'src>(
    input: LocatedSpan<&'src str>,
) -> IResult<LocatedSpan<&'src str>, Statement<'src>> {
    let (input, _) = delimited(multispace0, tag("break"), multispace0)(input)?;
    Ok((input, Statement::Break))
}

fn continue_statement<'src>(
    input: LocatedSpan<&'src str>,
) -> IResult<LocatedSpan<&'src str>, Statement<'src>> {
    let (input, _) = delimited(multispace0, tag("continue"), multispace0)(input)?;
    Ok((input, Statement::Continue))
}
