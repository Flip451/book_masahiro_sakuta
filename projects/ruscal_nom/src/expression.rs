use nom::{
    branch::alt,
    bytes::complete::tag,
    character::{
        complete::{alpha1, alphanumeric1, digit1, multispace0, multispace1, none_of},
        streaming::space0,
    },
    combinator::{opt, recognize},
    multi::{fold_many0, many0},
    number::complete::double,
    sequence::{delimited, pair, preceded, terminated},
    IResult,
};

use crate::{
    break_result::EvalResult, helper, stack_frame::StackFrame, statement::Statements, value::Value,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Ident<'src>(pub(crate) &'src str);

#[derive(Clone, Debug)]
pub enum Expression<'src> {
    Ident(Ident<'src>),
    Value(Value),
    FnInvoke(Ident<'src>, Vec<Expression<'src>>),
    If(
        Box<Expression<'src>>,
        Box<Statements<'src>>,
        Option<Box<Statements<'src>>>,
    ),
    Add(Box<Expression<'src>>, Box<Expression<'src>>),
    Sub(Box<Expression<'src>>, Box<Expression<'src>>),
    Mul(Box<Expression<'src>>, Box<Expression<'src>>),
    Div(Box<Expression<'src>>, Box<Expression<'src>>),
    Rem(Box<Expression<'src>>, Box<Expression<'src>>),
    Eq(Box<Expression<'src>>, Box<Expression<'src>>),
    Gt(Box<Expression<'src>>, Box<Expression<'src>>),
    Lt(Box<Expression<'src>>, Box<Expression<'src>>),
    Ge(Box<Expression<'src>>, Box<Expression<'src>>),
    Le(Box<Expression<'src>>, Box<Expression<'src>>),
    // Pow(Box<Expression<'src>>, Box<Expression<'src>>),
}

impl<'src> Expression<'src> {
    pub(crate) fn eval(&'src self, stack_frame: &mut StackFrame<'src>) -> EvalResult {
        let result: Value = match self {
            Expression::Value(v) => v.clone(),
            Expression::Ident(Ident("pi")) => Value::F64(std::f64::consts::PI),
            Expression::Ident(var) => stack_frame
                .get_variable(*var)
                .expect(&format!("variable {:?} not found", var)),
            Expression::If(cond, then, otherwise) => {
                if cond.eval(stack_frame)? == Value::Boolean(true) {
                    then.eval(stack_frame)?
                } else if let Some(otherwise) = otherwise {
                    otherwise.eval(stack_frame)?
                } else {
                    Value::EmptyTuple
                }
            }
            Expression::Add(lhs, rhs) => lhs.eval(stack_frame)? + rhs.eval(stack_frame)?,
            Expression::Sub(lhs, rhs) => lhs.eval(stack_frame)? - rhs.eval(stack_frame)?,
            Expression::Mul(lhs, rhs) => lhs.eval(stack_frame)? * rhs.eval(stack_frame)?,
            Expression::Div(lhs, rhs) => lhs.eval(stack_frame)? / rhs.eval(stack_frame)?,
            Expression::Rem(lhs, rhs) => lhs.eval(stack_frame)? % rhs.eval(stack_frame)?,
            Expression::FnInvoke(ident, args) => {
                let mut evaluated_args = vec![];
                for arg in args.iter() {
                    evaluated_args.push(arg.eval(stack_frame)?);
                }
                match stack_frame.get_function(*ident) {
                    Some(f) => f.call(&evaluated_args, stack_frame),
                    None => panic!("function {:?} not found", ident),
                }
            }
            Expression::Eq(lhs, rhs) => {
                if lhs.eval(stack_frame)? == rhs.eval(stack_frame)? {
                    Value::Boolean(true)
                } else {
                    Value::Boolean(false)
                }
            }
            Expression::Gt(lhs, rhs) => {
                if lhs.eval(stack_frame)? > rhs.eval(stack_frame)? {
                    Value::Boolean(true)
                } else {
                    Value::Boolean(false)
                }
            }
            Expression::Lt(lhs, rhs) => {
                if lhs.eval(stack_frame)? < rhs.eval(stack_frame)? {
                    Value::Boolean(true)
                } else {
                    Value::Boolean(false)
                }
            }
            Expression::Ge(lhs, rhs) => {
                if lhs.eval(stack_frame)? >= rhs.eval(stack_frame)? {
                    Value::Boolean(true)
                } else {
                    Value::Boolean(false)
                }
            }
            Expression::Le(lhs, rhs) => {
                if lhs.eval(stack_frame)? <= rhs.eval(stack_frame)? {
                    Value::Boolean(true)
                } else {
                    Value::Boolean(false)
                }
            }
        };
        EvalResult::Continue(result)
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

// 浮動小数点数をパースする
fn double_number<'src>(input: &'src str) -> IResult<&'src str, Value> {
    let (input, float) = delimited(multispace0, double, multispace0)(input)?;
    Ok((input, Value::F64(float)))
}

// 整数をパースする
fn int_number<'src>(input: &'src str) -> IResult<&'src str, Value> {
    let (input, int) = delimited(
        multispace0,
        recognize(preceded(opt(tag("-")), digit1)),
        multispace0,
    )(input)?;
    let int = int
        .parse::<i64>()
        .expect(&format!("invalid integer: {int}"));
    Ok((input, Value::I64(int)))
}

// 数値式をパースする
fn number_expr<'src>(input: &'src str) -> IResult<&'src str, Expression<'src>> {
    let (rest_double, double) = double_number(input)?;
    let (rest_int, int) = int_number(input)?;
    let (input, value) = if rest_double == rest_int {
        (rest_int, int)
    } else {
        (rest_double, double)
    };
    Ok((input, Expression::Value(value)))
}

// 文字列リテラルをパースする
fn string_literal<'src>(input: &'src str) -> IResult<&'src str, Value> {
    let (input, _) = preceded(multispace0, tag("\""))(input)?;
    let (input, string) = many0(none_of("\""))(input)?;
    let (input, _) = terminated(tag("\""), multispace0)(input)?;
    Ok((
        input,
        Value::String(
            string
                .iter()
                .collect::<String>()
                .replace("\\\\", "\\")
                .replace("\\n", "\n"),
        ),
    ))
}

// 文字列式をパースする
fn string_expr<'src>(input: &'src str) -> IResult<&'src str, Expression<'src>> {
    let (input, value) = string_literal(input)?;
    Ok((input, Expression::Value(value)))
}

// 真偽値をパースする
fn boolean<'src>(input: &'src str) -> IResult<&'src str, Value> {
    let (input, value) = alt((tag("true"), tag("false")))(input)?;
    let value = if value == "true" {
        Value::Boolean(true)
    } else if value == "false" {
        Value::Boolean(false)
    } else {
        unreachable!("invalid boolean literal: {value}")
    };
    Ok((input, value))
}

// 真偽値式をパースする
fn boolean_expr<'src>(input: &'src str) -> IResult<&'src str, Expression<'src>> {
    let (input, value) = boolean(input)?;
    Ok((input, Expression::Value(value)))
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

// 識別子式をパースする
fn ident_expr<'src>(input: &'src str) -> IResult<&'src str, Expression<'src>> {
    let (input, ident) = ident(input)?;
    Ok((input, Expression::Ident(ident)))
}

// 値をパースする
fn value<'src>(input: &'src str) -> IResult<&'src str, Expression<'src>> {
    let (input, value) = alt((number_expr, string_expr, boolean_expr, ident_expr))(input)?;
    Ok((input, value))
}

pub fn expr<'src>(input: &'src str) -> IResult<&'src str, Expression<'src>> {
    alt((if_expr, comparison_expr, num_expr))(input)
}

fn if_expr<'src>(input: &'src str) -> IResult<&'src str, Expression<'src>> {
    let (input, _) = delimited(multispace0, tag("if"), multispace1)(input)?;
    let (input, cond) = expr(input)?;
    let (input, _) = helper::open_brace(input)?;
    let (input, then) = Statements::parse(input)?;
    let (input, _) = helper::close_brace(input)?;
    let (input, otherwise) = opt(preceded(
        delimited(multispace0, tag("else"), multispace0),
        delimited(helper::open_brace, Statements::parse, helper::close_brace),
    ))(input)?;
    Ok((
        input,
        Expression::If(Box::new(cond), Box::new(then), otherwise.map(Box::new)),
    ))
}

fn comparison_expr<'src>(input: &'src str) -> IResult<&'src str, Expression<'src>> {
    let (input, lhs) = alt((if_expr, num_expr))(input)?;
    let (input, op) = alt((tag(">="), tag("<="), tag(">"), tag("<"), tag("==")))(input)?;
    let (input, rhs) = alt((if_expr, num_expr))(input)?;
    Ok((
        input,
        match op {
            ">" => Expression::Gt(Box::new(lhs), Box::new(rhs)),
            "<" => Expression::Lt(Box::new(lhs), Box::new(rhs)),
            ">=" => Expression::Ge(Box::new(lhs), Box::new(rhs)),
            "<=" => Expression::Le(Box::new(lhs), Box::new(rhs)),
            "==" => Expression::Eq(Box::new(lhs), Box::new(rhs)),
            _ => unreachable!("invalid comparison operator"),
        },
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
