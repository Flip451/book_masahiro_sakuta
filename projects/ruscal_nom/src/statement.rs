use nom::{bytes::complete::tag, error::Error, multi::separated_list0, Finish};

use crate::expression::{self, Expression};

type Statements<'src> = Vec<Expression<'src>>;

pub fn statements(input: &str) -> Result<Statements, Error<&str>> {
    let (input, statements) = separated_list0(tag(";"), expression::expr)(input).finish()?;
    Ok(statements)
}
