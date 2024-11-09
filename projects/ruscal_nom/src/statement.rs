use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{multispace0, multispace1},
    multi::{many0, separated_list0},
    sequence::{delimited, preceded, terminated},
    IResult,
};

use crate::{
    break_result::{BreakResult, EvalResult},
    expression::{self, Expression, Ident},
    function::{FnDef, UserFn},
    helper,
    stack_frame::StackFrame,
    type_check::{self, TypeCheckContext, TypeCheckError, TypeDeclare},
    value::Value,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<'src> {
    VarDef(Ident<'src>, TypeDeclare, Expression<'src>),
    Assignment(Ident<'src>, Expression<'src>),
    Expression(Expression<'src>),
    For {
        loop_var: Ident<'src>,
        start: Expression<'src>,
        end: Expression<'src>,
        body: Statements<'src>,
    },
    FnDef {
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
    pub fn parse(input: &'src str) -> IResult<&'src str, Statement<'src>> {
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
    pub fn parse(input: &'src str) -> IResult<&'src str, Statements<'src>> {
        let (input, statements) = many0(Statement::parse)(input)?;
        Ok((input, Statements(statements)))
    }

    pub fn eval(&'src self, stack_frame: &mut StackFrame<'src>) -> EvalResult {
        let mut result = EvalResult::Continue(Value::I64(0));
        for statement in self.0.iter() {
            match statement {
                Statement::VarDef(ident, _type_declare, expr) => {
                    let value = expr.eval(stack_frame)?;
                    stack_frame.insert_variable(*ident, value);
                }
                Statement::Assignment(ident, expression) => {
                    if stack_frame.get_variable(*ident).is_none() {
                        panic!("Error: {:?} is not defined", ident);
                    }
                    let value = expression.eval(stack_frame)?;
                    stack_frame.insert_variable(*ident, value);
                }
                Statement::Expression(expr) => {
                    result = EvalResult::Continue(expr.eval(stack_frame)?);
                }
                Statement::For {
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
        context: &mut TypeCheckContext<'src>,
    ) -> Result<TypeDeclare, TypeCheckError> {
        let mut result = TypeDeclare::EmptyTuple;
        for statement in self.0.iter() {
            result = match statement {
                Statement::VarDef(ident, type_declare, expression) => {
                    let expr_type = expression.type_check(context)?;
                    type_declare.coerce_type(&expr_type)?;
                    context.insert_variable(*ident, expr_type);
                    TypeDeclare::EmptyTuple
                }
                Statement::Assignment(ident, expression) => {
                    let expr_type = expression.type_check(context)?;
                    let var_type = context
                        .get_variable(ident)
                        .ok_or(TypeCheckError::UndefinedVariable(ident.to_string()))?;
                    var_type.coerce_type(&expr_type)?;
                    TypeDeclare::EmptyTuple
                }
                Statement::Expression(expression) => expression.type_check(context)?,
                Statement::For {
                    loop_var,
                    start,
                    end,
                    body,
                } => {
                    let start_type = start.type_check(context)?;
                    let end_type = end.type_check(context)?;
                    start_type.coerce_type(&TypeDeclare::I64)?;
                    end_type.coerce_type(&TypeDeclare::I64)?;
                    context.insert_variable(*loop_var, TypeDeclare::I64);
                    body.type_check(context)? // TODO: ループ内変数を削除する、 type_check の返り値を ControlFlow に置き換える
                }
                Statement::FnDef {
                    name,
                    params,
                    return_type,
                    body,
                } => {
                    let mut sub_context= TypeCheckContext::push(context);
                    for (ident, type_declare) in params {
                        sub_context.insert_variable(*ident, *type_declare);
                    }
                    let body_type = body.type_check(&mut sub_context)?;
                    body_type.coerce_type(&return_type)?;
                    // コンテキストへの関数の代入
                    let fn_def = FnDef::User(UserFn::new(&params[..], *return_type, &body));
                    context.insert_function(*name, fn_def);
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

fn terminator<'src>(input: &'src str) -> IResult<&'src str, ()> {
    let (input, _) = delimited(multispace0, tag(";"), multispace0)(input)?;
    Ok((input, ()))
}

fn var_def<'src>(input: &'src str) -> IResult<&'src str, Statement<'src>> {
    let (input, _) = delimited(multispace0, tag("let"), multispace1)(input)?;
    let (input, name) = expression::ident(input)?;
    let (input, _) = delimited(multispace0, tag(":"), multispace0)(input)?;
    let (input, type_declare) = type_check::TypeDeclare::parse(input)?;
    let (input, _) = delimited(multispace0, tag("="), multispace0)(input)?;
    let (input, expr) = expression::expr(input)?;
    Ok((input, Statement::VarDef(name, type_declare, expr)))
}

fn assignment<'src>(input: &'src str) -> IResult<&'src str, Statement<'src>> {
    let (input, name) = expression::ident(input)?;
    let (input, _) = delimited(multispace0, tag("="), multispace0)(input)?;
    let (input, expr) = expression::expr(input)?;
    Ok((input, Statement::Assignment(name, expr)))
}

fn expr_statement<'src>(input: &'src str) -> IResult<&'src str, Statement<'src>> {
    let (input, expr) = expression::expr(input)?;
    Ok((input, Statement::Expression(expr)))
}

fn for_statement<'src>(input: &'src str) -> IResult<&'src str, Statement<'src>> {
    let (input, _) = delimited(multispace0, tag("for"), multispace1)(input)?;
    let (input, loop_var) = expression::ident(input)?;
    let (input, _) = delimited(multispace0, tag("in"), multispace0)(input)?;
    let (input, start) = expression::expr(input)?;
    let (input, _) = delimited(multispace0, tag("to"), multispace0)(input)?;
    let (input, end) = expression::expr(input)?;
    let (input, body) =
        delimited(helper::open_brace, Statements::parse, helper::close_brace)(input)?;
    Ok((
        input,
        Statement::For {
            loop_var,
            start,
            end,
            body,
        },
    ))
}

fn fn_def<'src>(input: &'src str) -> IResult<&'src str, Statement<'src>> {
    let (input, _) = delimited(multispace0, tag("fn"), multispace1)(input)?;
    let (input, name) = expression::ident(input)?;
    let (input, params) = delimited(
        multispace0,
        delimited(tag("("), separated_list0(tag(","), parameter), tag(")")),
        multispace0,
    )(input)?;
    let (input, return_type) = preceded(
        delimited(multispace0, tag("->"), multispace0),
        TypeDeclare::parse,
    )(input)?;
    let (input, body) =
        delimited(helper::open_brace, Statements::parse, helper::close_brace)(input)?;
    Ok((
        input,
        Statement::FnDef {
            name,
            params,
            return_type,
            body,
        },
    ))
}

fn parameter<'src>(input: &'src str) -> IResult<&'src str, (Ident<'src>, TypeDeclare)> {
    let (input, name) = expression::ident(input)?;
    let (input, _) = delimited(multispace0, tag(":"), multispace0)(input)?;
    let (input, type_declare) = TypeDeclare::parse(input)?;
    Ok((input, (name, type_declare)))
}

fn return_statement<'src>(input: &'src str) -> IResult<&'src str, Statement<'src>> {
    let (input, _) = delimited(multispace0, tag("return"), multispace1)(input)?;
    let (input, expr) = expression::expr(input)?;
    Ok((input, Statement::Return(expr)))
}

fn break_statement<'src>(input: &'src str) -> IResult<&'src str, Statement<'src>> {
    let (input, _) = delimited(multispace0, tag("break"), multispace0)(input)?;
    Ok((input, Statement::Break))
}

fn continue_statement<'src>(input: &'src str) -> IResult<&'src str, Statement<'src>> {
    let (input, _) = delimited(multispace0, tag("continue"), multispace0)(input)?;
    Ok((input, Statement::Continue))
}

#[cfg(test)]
mod tests {
    use std::error::Error;

    use super::*;

    #[test]
    fn test_var_def() -> Result<(), Box<dyn Error>> {
        let input = r#"
let a: i64 = 1;
let b:f64=2;
"#;
        let result = Statements::parse(input)?;
        // assert_eq!(result.0, "");
        assert_eq!(
            result.1 .0[0],
            Statement::VarDef(
                Ident("a"),
                TypeDeclare::I64,
                Expression::Value(Value::I64(1))
            )
        );
        assert_eq!(
            result.1 .0[1],
            Statement::VarDef(
                Ident("b"),
                TypeDeclare::F64,
                Expression::Value(Value::F64(2.0))
            )
        );
        Ok(())
    }
}
