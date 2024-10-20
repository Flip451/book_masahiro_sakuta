use std::{collections::HashMap, error::Error};

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{multispace0, multispace1},
    multi::many0,
    sequence::{delimited, terminated},
    IResult,
};

use crate::expression::{self, Expression, Ident};

pub struct Statements<'src>(Vec<Statement<'src>>);

impl<'src> Statements<'src> {
    pub fn parse(input: &'src str) -> IResult<&'src str, Statements<'src>> {
        let (input, statements) = many0(Statement::parse)(input)?;
        Ok((input, Statements(statements)))
    }

    pub fn eval(&self, variables: &mut HashMap<Ident<'src>, f64>) -> Result<(), Box<dyn Error>> {
        for statement in self.0.iter() {
            match statement {
                Statement::VarDef(ref ident, expr) => {
                    println!("{:?} = {:?}", ident, expr);
                    let value = expr.eval(variables);
                    variables.insert(*ident, value);
                }
                Statement::Assignment(ident, expression) => {
                    println!("{:?} = {:?}", ident, expression);
                    if !variables.contains_key(&ident) {
                        panic!("Error: {:?} is not defined", ident);
                    }
                    variables.insert(*ident, expression.eval(variables));
                }
                Statement::Expression(expr) => {
                    println!("{}", expr.eval(&variables));
                }
                Statement::For {
                    loop_var,
                    start,
                    end,
                    body,
                } => {
                    let start = start.eval(variables) as isize;
                    let end = end.eval(variables) as isize;
                    for i in start..end {
                        variables.insert(*loop_var, i as f64);
                        body.eval(variables)?;
                    }
                }
            }
        }
        Ok(())
    }
}

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
}

impl<'src> Statement<'src> {
    pub fn parse(input: &'src str) -> IResult<&'src str, Statement<'src>> {
        let (input, statement) = alt((
            for_statement,
            terminated(
                alt((var_def, assignment, expr_statement)),
                delimited(multispace0, tag(";"), multispace0),
            ),
        ))(input)?;
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
    let (input, body) = delimited(open_brace, Statements::parse, close_brace)(input)?;
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

pub(crate) fn open_brace<'src>(input: &'src str) -> IResult<&'src str, ()> {
    let (input, _) = delimited(multispace0, tag("{"), multispace0)(input)?;
    Ok((input, ()))
}

pub(crate) fn close_brace<'src>(input: &'src str) -> IResult<&'src str, ()> {
    let (input, _) = delimited(multispace0, tag("}"), multispace0)(input)?;
    Ok((input, ()))
}
