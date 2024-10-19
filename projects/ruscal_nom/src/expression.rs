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

#[derive(Clone, Debug)]
pub enum Token<'src> {
    Ident(&'src str),
    Number(f64),
}

#[derive(Clone, Debug)]
pub enum Expression<'src> {
    Value(Token<'src>),
    FnInvoke(Token<'src>, Vec<Expression<'src>>),
    Add(Box<Expression<'src>>, Box<Expression<'src>>),
    Sub(Box<Expression<'src>>, Box<Expression<'src>>),
    Mul(Box<Expression<'src>>, Box<Expression<'src>>),
    Div(Box<Expression<'src>>, Box<Expression<'src>>),
    Rem(Box<Expression<'src>>, Box<Expression<'src>>),
    // Pow(Box<Expression<'src>>, Box<Expression<'src>>),
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
            Expression::FnInvoke(Token::Ident("sqrt"), args) => unary_fn(f64::sqrt)(args),
            Expression::FnInvoke(Token::Ident("sin"), args) => unary_fn(f64::sin)(args),
            Expression::FnInvoke(Token::Ident("cos"), args) => unary_fn(f64::cos)(args),
            Expression::FnInvoke(Token::Ident("tan"), args) => unary_fn(f64::tan)(args),
            Expression::FnInvoke(Token::Ident("asin"), args) => unary_fn(f64::asin)(args),
            Expression::FnInvoke(Token::Ident("acos"), args) => unary_fn(f64::acos)(args),
            Expression::FnInvoke(Token::Ident("atan"), args) => unary_fn(f64::atan)(args),
            Expression::FnInvoke(Token::Ident("atan2"), args) => binary_fn(f64::atan2)(args),
            Expression::FnInvoke(Token::Ident("pow"), args) => binary_fn(f64::powf)(args),
            Expression::FnInvoke(Token::Ident("exp"), args) => unary_fn(f64::exp)(args),
            Expression::FnInvoke(Token::Ident("ln"), args) => unary_fn(f64::ln)(args),
            Expression::FnInvoke(Token::Ident("log10"), args) => unary_fn(f64::log10)(args),
            Expression::FnInvoke(Token::Ident("log2"), args) => unary_fn(f64::log2)(args),
            Expression::FnInvoke(Token::Ident("log"), args) => binary_fn(f64::log)(args),
            Expression::FnInvoke(Token::Ident(name), _) => {
                panic!("Function call to {:?} is not implemented", name)
            }
            Expression::FnInvoke(Token::Number(_), _) => {
                panic!("Function call with number is not allowed")
            } // Expression::Pow(lhs, rhs) => lhs.eval().powf(rhs.eval()),
        }
    }
}

// 単項関数を式の配列に対する関数に変換する関数
fn unary_fn(f: impl Fn(f64) -> f64) -> impl Fn(&Vec<Expression>) -> f64 {
    move |args| {
        let arg = args.first().expect("function missing argument");
        let arg = arg.eval();
        f(arg)
    }
}

// 二項関数を式の配列に対する関数に変換する関数
fn binary_fn(f: impl Fn(f64, f64) -> f64) -> impl Fn(&Vec<Expression>) -> f64 {
    move |args| {
        let first_arg = args.first().expect("function missing the first argument");
        let first_arg = first_arg.eval();

        let second_arg = args.get(1).expect("function missing the second argument");
        let second_arg = second_arg.eval();

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
fn ident<'src>(input: &'src str) -> IResult<&'src str, Token<'src>> {
    let (input, ident) = delimited(
        multispace0,
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_")))),
        )),
        multispace0,
    )(input)?;
    Ok((input, Token::Ident(ident)))
}

// 値をパースする
fn value<'src>(input: &'src str) -> IResult<&'src str, Expression<'src>> {
    let (input, token) = alt((number, ident))(input)?;
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
