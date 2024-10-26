use std::ops::ControlFlow;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{multispace0, multispace1},
    multi::{many0, separated_list0},
    sequence::{delimited, terminated},
    IResult,
};

use crate::{
    expression::{self, Expression, Ident},
    function::{self, FnDef},
    helper,
    stack_frame::StackFrame,
};

#[derive(Debug, Clone)]
pub struct Statements<'src>(Vec<Statement<'src>>);

type EvalResult = ControlFlow<f64, f64>;

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
                        body.eval(stack_frame)?;
                    }
                }
                Statement::FnDef { name, params, body } => {
                    let fn_def = FnDef::User(function::UserFn::new(&params[..], &body));
                    stack_frame.insert_function(*name, fn_def);
                }
                Statement::Return(expression) => {
                    result = EvalResult::Break(expression.eval(stack_frame)?);
                }
            }
        }
        result
    }
}

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
}

impl<'src> Statement<'src> {
    pub fn parse(input: &'src str) -> IResult<&'src str, Statement<'src>> {
        let (input, statement) = alt((
            for_statement,
            fn_def,
            terminated(
                alt((return_statement, var_def, assignment, expr_statement)),
                delimited(multispace0, tag(";"), multispace0),
            ),
        ))(input)?;
        println!("{:?}", statement);
        Ok((input, statement))
    }
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
