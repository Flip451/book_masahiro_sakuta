use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{multispace0, multispace1},
    multi::{many0, separated_list0},
    sequence::{delimited, terminated},
    IResult,
};

use crate::{
    break_result::{BreakResult, EvalResult},
    expression::{self, Expression, Ident},
    function::{self, FnDef},
    helper,
    stack_frame::StackFrame,
};

#[derive(Debug, Clone)]
pub enum Statement<'src> {
    VarDef(Ident<'src>, Expression<'src>),
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
        params: Vec<Ident<'src>>,
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
        println!("{:?}", statement);
        Ok((input, statement))
    }
}

#[derive(Debug, Clone)]
pub struct Statements<'src>(Vec<Statement<'src>>);

impl<'src> Statements<'src> {
    pub fn parse(input: &'src str) -> IResult<&'src str, Statements<'src>> {
        let (input, statements) = many0(Statement::parse)(input)?;
        Ok((input, Statements(statements)))
    }

    pub fn eval(&'src self, stack_frame: &mut StackFrame<'src>) -> EvalResult {
        let mut result = EvalResult::Continue(0.0);
        for statement in self.0.iter() {
            match statement {
                Statement::VarDef(ident, expr) => {
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
                    let start = start.eval(stack_frame)? as isize;
                    let end = end.eval(stack_frame)? as isize;
                    for i in start..end {
                        stack_frame.insert_variable(*loop_var, i as f64);
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
                Statement::FnDef { name, params, body } => {
                    let fn_def = FnDef::User(function::UserFn::new(&params[..], &body));
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
}

fn terminator<'src>(input: &'src str) -> IResult<&'src str, ()> {
    let (input, _) = delimited(multispace0, tag(";"), multispace0)(input)?;
    Ok((input, ()))
}

fn var_def<'src>(input: &'src str) -> IResult<&'src str, Statement<'src>> {
    let (input, _) = delimited(multispace0, tag("let"), multispace1)(input)?;
    let (input, name) = expression::ident(input)?;
    let (input, _) = delimited(multispace0, tag("="), multispace0)(input)?;
    let (input, expr) = expression::expr(input)?;
    Ok((input, Statement::VarDef(name, expr)))
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
        delimited(
            tag("("),
            separated_list0(tag(","), expression::ident),
            tag(")"),
        ),
        multispace0,
    )(input)?;
    let (input, body) =
        delimited(helper::open_brace, Statements::parse, helper::close_brace)(input)?;
    Ok((input, Statement::FnDef { name, params, body }))
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
