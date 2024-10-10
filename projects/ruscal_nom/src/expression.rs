use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, multispace0},
    combinator::recognize,
    multi::{fold_many0, many0},
    number::complete::double,
    sequence::{delimited, pair},
    IResult,
};

#[derive(Clone, Debug)]
pub enum Token<'src> {
    Ident(&'src str),
    Number(f64),
}

#[derive(Clone, Debug)]
pub enum Expression<'src> {
    Value(Token<'src>),
    Add(Box<Expression<'src>>, Box<Expression<'src>>),
    Sub(Box<Expression<'src>>, Box<Expression<'src>>),
    Mul(Box<Expression<'src>>, Box<Expression<'src>>),
    Div(Box<Expression<'src>>, Box<Expression<'src>>),
    Rem(Box<Expression<'src>>, Box<Expression<'src>>),
    Pow(Box<Expression<'src>>, Box<Expression<'src>>),
}

impl<'src> Expression<'src> {
    pub fn eval(&self) -> f64 {
        match self {
            Expression::Value(Token::Number(n)) => *n,
            Expression::Value(Token::Ident("pi")) => std::f64::consts::PI,
            Expression::Value(Token::Ident(_)) => todo!(),
            Expression::Add(lhs, rhs) => lhs.eval() + rhs.eval(),
            Expression::Sub(lhs, rhs) => lhs.eval() - rhs.eval(),
            Expression::Mul(lhs, rhs) => lhs.eval() * rhs.eval(),
            Expression::Div(lhs, rhs) => lhs.eval() / rhs.eval(),
            Expression::Rem(lhs, rhs) => lhs.eval() % rhs.eval(),
            Expression::Pow(lhs, rhs) => lhs.eval().powf(rhs.eval()),
        }
    }
}

// 因子は数値リテラル または 識別子 または () で囲まれた式 のいずれかである
fn factor<'src>(input: &'src str) -> IResult<&'src str, Expression<'src>> {
    alt((number, ident, parens))(input)
}

// () で囲まれた式をパースする
fn parens<'src>(input: &'src str) -> IResult<&'src str, Expression<'src>> {
    delimited(
        multispace0,
        delimited(tag("("), expr, tag(")")),
        multispace0,
    )(input)
}

// 数値をパースする
fn number<'src>(input: &'src str) -> IResult<&'src str, Expression<'src>> {
    let (input, float) = delimited(multispace0, double, multispace0)(input)?;
    Ok((input, Expression::Value(Token::Number(float))))
}

// 識別子をパースする
fn ident<'src>(input: &'src str) -> IResult<&'src str, Expression<'src>> {
    let (input, ident) = delimited(
        multispace0,
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_")))),
        )),
        multispace0,
    )(input)?;
    Ok((input, Expression::Value(Token::Ident(ident))))
}

// 加算式をパースする
pub fn expr<'src>(input: &'src str) -> IResult<&'src str, Expression<'src>> {
    let (input, lhs) = term(input)?;

    fold_many0(
        pair(
            delimited(multispace0, alt((tag("+"), tag("-"))), multispace0),
            term,
        ),
        move || lhs.clone(),
        |lhs, (op, rhs)| match op {
            "+" => Expression::Add(Box::new(lhs), Box::new(rhs)),
            "-" => Expression::Sub(Box::new(lhs), Box::new(rhs)),
            _ => unreachable!("Multiplicative operator is not allowed in additive expression"),
        },
    )(input)
}

// 乗算式をパースする
fn term<'src>(input: &'src str) -> IResult<&'src str, Expression<'src>> {
    let (input, lhs) = factor(input)?;

    fold_many0(
        pair(
            delimited(
                multispace0,
                alt((tag("*"), tag("/"), tag("%"), tag("^"))),
                multispace0,
            ),
            factor,
        ),
        move || lhs.clone(),
        |lhs, (op, rhs)| match op {
            "*" => Expression::Mul(Box::new(lhs), Box::new(rhs)),
            "/" => Expression::Div(Box::new(lhs), Box::new(rhs)),
            "%" => Expression::Rem(Box::new(lhs), Box::new(rhs)),
            "^" => Expression::Pow(Box::new(lhs), Box::new(rhs)),
            _ => unreachable!("invalid multiplicative operator"),
        },
    )(input)
}
