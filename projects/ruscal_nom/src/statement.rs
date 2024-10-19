use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{multispace0, multispace1},
    error::Error,
    multi::separated_list0,
    sequence::delimited,
    Finish, IResult,
};

use crate::expression::{self, Expression, Ident};

type Statements<'src> = Vec<Statement<'src>>;

pub enum Statement<'src> {
    Expression(Expression<'src>),
    LetDef(Ident<'src>, Expression<'src>),
}

pub fn statements(input: &str) -> Result<Statements, Error<&str>> {
    let (_, statements) = separated_list0(tag(";"), statement)(input).finish()?;
    Ok(statements)
}

fn statement<'src>(input: &'src str) -> IResult<&'src str, Statement<'src>> {
    let (input, statement) = alt((var_def, expr_statement))(input)?;
    Ok((input, statement))
}

fn var_def<'src>(input: &'src str) -> IResult<&'src str, Statement<'src>> {
    let (input, _) = delimited(multispace0, tag("let"), multispace1)(input)?;
    let (input, name) = expression::ident(input)?;
    let (input, _) = delimited(multispace0, tag("="), multispace0)(input)?;
    let (input, expr) = expression::expr(input)?;
    Ok((input, Statement::LetDef(name, expr)))
}

fn expr_statement<'src>(input: &'src str) -> IResult<&'src str, Statement<'src>> {
    let (input, expr) = expression::expr(input)?;
    Ok((input, Statement::Expression(expr)))
}
