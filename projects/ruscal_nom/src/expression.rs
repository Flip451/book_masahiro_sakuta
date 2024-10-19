use std::collections::HashMap;

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
    sequence::{delimited, pair},
    IResult,
};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Ident<'src>(&'src str);

#[derive(Clone, Debug)]
pub enum Token<'src> {
    Ident(Ident<'src>),
    Number(f64),
}

#[derive(Clone, Debug)]
pub enum Expression<'src> {
    Value(Token<'src>),
    FnInvoke(Ident<'src>, Vec<Expression<'src>>),
    Add(Box<Expression<'src>>, Box<Expression<'src>>),
    Sub(Box<Expression<'src>>, Box<Expression<'src>>),
    Mul(Box<Expression<'src>>, Box<Expression<'src>>),
    Div(Box<Expression<'src>>, Box<Expression<'src>>),
    Rem(Box<Expression<'src>>, Box<Expression<'src>>),
    // Pow(Box<Expression<'src>>, Box<Expression<'src>>),
}

impl<'src> Expression<'src> {
    pub fn eval(&self, variables: &HashMap<Ident<'src>, f64>) -> f64 {
        match self {
            Expression::Value(Token::Number(n)) => *n,
            Expression::Value(Token::Ident(Ident("pi"))) => std::f64::consts::PI,
            Expression::Value(Token::Ident(var)) => *variables.get(var).expect("variable not found"),
            Expression::Add(lhs, rhs) => lhs.eval(variables) + rhs.eval(variables),
            Expression::Sub(lhs, rhs) => lhs.eval(variables) - rhs.eval(variables),
            Expression::Mul(lhs, rhs) => lhs.eval(variables) * rhs.eval(variables),
            Expression::Div(lhs, rhs) => lhs.eval(variables) / rhs.eval(variables),
            Expression::Rem(lhs, rhs) => lhs.eval(variables) % rhs.eval(variables),
            // Expression::Pow(lhs, rhs) => lhs.eval().powf(rhs.eval()),
            Expression::FnInvoke(Ident("sqrt"), args) => unary_fn(f64::sqrt)(args, variables),
            Expression::FnInvoke(Ident("sin"), args) => unary_fn(f64::sin)(args, variables),
            Expression::FnInvoke(Ident("cos"), args) => unary_fn(f64::cos)(args, variables),
            Expression::FnInvoke(Ident("tan"), args) => unary_fn(f64::tan)(args, variables),
            Expression::FnInvoke(Ident("asin"), args) => unary_fn(f64::asin)(args, variables),
            Expression::FnInvoke(Ident("acos"), args) => unary_fn(f64::acos)(args, variables),
            Expression::FnInvoke(Ident("atan"), args) => unary_fn(f64::atan)(args, variables),
            Expression::FnInvoke(Ident("atan2"), args) => binary_fn(f64::atan2)(args, variables),
            Expression::FnInvoke(Ident("pow"), args) => binary_fn(f64::powf)(args, variables),
            Expression::FnInvoke(Ident("exp"), args) => unary_fn(f64::exp)(args, variables),
            Expression::FnInvoke(Ident("ln"), args) => unary_fn(f64::ln)(args, variables),
            Expression::FnInvoke(Ident("log10"), args) => unary_fn(f64::log10)(args, variables),
            Expression::FnInvoke(Ident("log2"), args) => unary_fn(f64::log2)(args, variables),
            Expression::FnInvoke(Ident("log"), args) => binary_fn(f64::log)(args, variables),
            Expression::FnInvoke(Ident(name), _) => {
                panic!("Function call to {:?} is not implemented", name)
            }
        }
    }
}

// 単項関数を式の配列に対する関数に変換する関数
fn unary_fn<'src>(f: impl Fn(f64) -> f64) -> impl Fn(&Vec<Expression>, &HashMap<Ident<'src>, f64>) -> f64 {
    move |args, variables| {
        let arg = args.first().expect("function missing argument");
        let arg = arg.eval(variables);
        f(arg)
    }
}

// 二項関数を式の配列に対する関数に変換する関数
fn binary_fn<'src>(f: impl Fn(f64, f64) -> f64) -> impl Fn(&Vec<Expression>, &HashMap<Ident<'src>, f64>) -> f64 {
    move |args , variables| {
        let first_arg = args.first().expect("function missing the first argument");
        let first_arg = first_arg.eval(variables);

        let second_arg = args.get(1).expect("function missing the second argument");
        let second_arg = second_arg.eval(variables);

        f(first_arg, second_arg)
    }
}

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

// 式（加算式）をパースする
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
