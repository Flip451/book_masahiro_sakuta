use nom::{
    branch::alt,
    bytes::complete::tag,
    character::{
        complete::{alpha1, alphanumeric1, multispace0},
        streaming::space0,
    },
    combinator::{opt, recognize},
    multi::{fold_many0, many0},
    number::complete::double,
    sequence::{delimited, pair, preceded},
    IResult,
};

use crate::{stack_frame::StackFrame, statement};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Ident<'src>(pub(crate) &'src str);

#[derive(Clone, Debug)]
pub enum Token<'src> {
    Ident(Ident<'src>),
    Number(f64),
}

#[derive(Clone, Debug)]
pub enum Expression<'src> {
    Value(Token<'src>),
    FnInvoke(Ident<'src>, Vec<Expression<'src>>),
    If(
        Box<Expression<'src>>,
        Box<Expression<'src>>,
        Option<Box<Expression<'src>>>,
    ),
    Add(Box<Expression<'src>>, Box<Expression<'src>>),
    Sub(Box<Expression<'src>>, Box<Expression<'src>>),
    Mul(Box<Expression<'src>>, Box<Expression<'src>>),
    Div(Box<Expression<'src>>, Box<Expression<'src>>),
    Rem(Box<Expression<'src>>, Box<Expression<'src>>),
    // Pow(Box<Expression<'src>>, Box<Expression<'src>>),
}

impl<'src> Expression<'src> {
    pub fn eval(&'src self, stack_frame: &StackFrame<'src>) -> f64 {
        match self {
            Expression::Value(Token::Number(n)) => *n,
            Expression::Value(Token::Ident(var)) => stack_frame
                .get_variable(*var)
                .expect(&format!("variable {:?} not found", var)),
            Expression::If(cond, then, otherwise) => {
                if cond.eval(stack_frame) != 0.0 {
                    then.eval(stack_frame)
                } else if let Some(otherwise) = otherwise {
                    otherwise.eval(stack_frame)
                } else {
                    0.0
                }
            }
            Expression::Add(lhs, rhs) => lhs.eval(stack_frame) + rhs.eval(stack_frame),
            Expression::Sub(lhs, rhs) => lhs.eval(stack_frame) - rhs.eval(stack_frame),
            Expression::Mul(lhs, rhs) => lhs.eval(stack_frame) * rhs.eval(stack_frame),
            Expression::Div(lhs, rhs) => lhs.eval(stack_frame) / rhs.eval(stack_frame),
            Expression::Rem(lhs, rhs) => lhs.eval(stack_frame) % rhs.eval(stack_frame),
            Expression::FnInvoke(ident, args) => {
                let evaluated_args: Vec<_> = args.iter().map(|arg| arg.eval(stack_frame)).collect();
                match stack_frame.get_function(*ident) {
                    Some(f) => f.call(&evaluated_args, stack_frame),
                    None => panic!("function {:?} not found", ident),
                }
            }
        }
    }
}

// 要素の左右の空白と改行を無視するための高階関数
// fn space_delimited<'src, F, O, E>(f: F) -> impl FnMut(&'src str) -> IResult<&'src str, O, E>
// where
//     F: Parser<&'src str, O, E>,
//     E: ParseError<&'src str>,
// {
//     delimited(
//         many0(alt((space1, line_ending))),
//         f,
//         many0(alt((space1, line_ending))),
//     )
// }

// 因子は数値リテラル または 識別子 または () で囲まれた式、関数呼び出し のいずれかである
fn factor<'src>(input: &'src str) -> IResult<&'src str, Expression<'src>> {
    alt((function_call, value, parens))(input)
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
fn number<'src>(input: &'src str) -> IResult<&'src str, Token<'src>> {
    let (input, float) = delimited(multispace0, double, multispace0)(input)?;
    Ok((input, Token::Number(float)))
}

// 識別子をパースする
pub(crate) fn ident<'src>(input: &'src str) -> IResult<&'src str, Ident<'src>> {
    let (input, ident) = delimited(
        multispace0,
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_")))),
        )),
        multispace0,
    )(input)?;
    Ok((input, Ident(ident)))
}

fn ident_token<'src>(input: &'src str) -> IResult<&'src str, Token<'src>> {
    let (input, ident) = ident(input)?;
    Ok((input, Token::Ident(ident)))
}

// 値をパースする
fn value<'src>(input: &'src str) -> IResult<&'src str, Expression<'src>> {
    let (input, token) = alt((number, ident_token))(input)?;
    Ok((input, Expression::Value(token)))
}

pub fn expr<'src>(input: &'src str) -> IResult<&'src str, Expression<'src>> {
    alt((if_expr, num_expr))(input)
}

fn if_expr<'src>(input: &'src str) -> IResult<&'src str, Expression<'src>> {
    let (input, _) = delimited(multispace0, tag("if"), multispace0)(input)?;
    let (input, cond) = expr(input)?;
    let (input, _) = statement::open_brace(input)?;
    let (input, then) = expr(input)?;
    let (input, _) = statement::close_brace(input)?;
    let (input, otherwise) = opt(preceded(
        delimited(multispace0, tag("else"), multispace0),
        delimited(statement::open_brace, expr, statement::close_brace),
    ))(input)?;
    Ok((
        input,
        Expression::If(Box::new(cond), Box::new(then), otherwise.map(Box::new)),
    ))
}

// 式（加算式）をパースする
fn num_expr<'src>(input: &'src str) -> IResult<&'src str, Expression<'src>> {
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

// 項（乗算式）をパースする
fn term<'src>(input: &'src str) -> IResult<&'src str, Expression<'src>> {
    let (input, lhs) = factor(input)?;

    fold_many0(
        pair(
            delimited(
                multispace0,
                alt((tag("*"), tag("/"), tag("%") /*, tag("^")*/)),
                multispace0,
            ),
            factor,
        ),
        move || lhs.clone(),
        |lhs, (op, rhs)| match op {
            "*" => Expression::Mul(Box::new(lhs), Box::new(rhs)),
            "/" => Expression::Div(Box::new(lhs), Box::new(rhs)),
            "%" => Expression::Rem(Box::new(lhs), Box::new(rhs)),
            // "^" => Expression::Pow(Box::new(lhs), Box::new(rhs)),
            _ => unreachable!("invalid multiplicative operator"),
        },
    )(input)
}

// 関数呼び出しをパースする
fn function_call<'src>(input: &'src str) -> IResult<&'src str, Expression<'src>> {
    let (input, ident) = ident(input)?;

    let (input, args) = delimited(
        multispace0,
        delimited(
            tag("("),
            many0(delimited(
                space0,
                expr,
                delimited(multispace0, opt(tag(",")), multispace0),
            )),
            tag(")"),
        ),
        multispace0,
    )(input)?;

    Ok((input, Expression::FnInvoke(ident, args)))
}
