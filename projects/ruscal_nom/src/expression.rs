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
use nom_locate::LocatedSpan;

use crate::{
    break_result::EvalResult,
    helper::{self},
    stack_frame::StackFrame,
    statement::Statements,
    type_check::{
        type_check_binary_op, BinaryOp, Span, TypeCheckContext, TypeCheckError, TypeDeclare,
    },
    value::Value,
};

use itertools::Itertools;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Ident<'src>(pub(crate) &'src str);

impl<'src> ToString for Ident<'src> {
    fn to_string(&self) -> String {
        self.0.to_string()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExpressionEnum<'src> {
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

#[derive(Clone, Debug, PartialEq)]
pub struct Expression<'src> {
    span: Span<'src>,
    inner: ExpressionEnum<'src>,
}

impl<'src> Expression<'src> {
    fn new(span: Span<'src>, inner: ExpressionEnum<'src>) -> Self {
        Self { span, inner }
    }

    fn located_span(&self) -> LocatedSpan<&'src str> {
        self.span.into_inner()
    }

    fn span(&self) -> Span<'src> {
        self.span
    }

    pub(crate) fn eval(&'src self, stack_frame: &mut StackFrame<'src>) -> EvalResult {
        let result: Value = match self {
            Expression {
                span: _,
                inner: ExpressionEnum::Value(v),
            } => v.clone(),
            Expression {
                span,
                inner: ExpressionEnum::Ident(var),
            } => stack_frame
                .get_variable(*var)
                .expect(&format!("{}\nvariable {:?} not found", span, var)),
            Expression {
                span: _,
                inner: ExpressionEnum::If(cond, then, otherwise),
            } => {
                if cond.eval(stack_frame)? == Value::Boolean(true) {
                    then.eval(stack_frame)?
                } else if let Some(otherwise) = otherwise {
                    otherwise.eval(stack_frame)?
                } else {
                    Value::EmptyTuple
                }
            }
            Expression {
                span: _,
                inner: ExpressionEnum::Add(lhs, rhs),
            } => lhs.eval(stack_frame)? + rhs.eval(stack_frame)?,
            Expression {
                span: _,
                inner: ExpressionEnum::Sub(lhs, rhs),
            } => lhs.eval(stack_frame)? - rhs.eval(stack_frame)?,
            Expression {
                span: _,
                inner: ExpressionEnum::Mul(lhs, rhs),
            } => lhs.eval(stack_frame)? * rhs.eval(stack_frame)?,
            Expression {
                span: _,
                inner: ExpressionEnum::Div(lhs, rhs),
            } => lhs.eval(stack_frame)? / rhs.eval(stack_frame)?,
            Expression {
                span: _,
                inner: ExpressionEnum::Rem(lhs, rhs),
            } => lhs.eval(stack_frame)? % rhs.eval(stack_frame)?,
            Expression {
                span,
                inner: ExpressionEnum::FnInvoke(ident, args),
            } => {
                let mut evaluated_args = vec![];
                for arg in args.iter() {
                    evaluated_args.push(arg.eval(stack_frame)?);
                }
                match stack_frame.get_function(*ident) {
                    Some(f) => f.call(&evaluated_args, stack_frame),
                    None => panic!("{}\nfunction {:?} not found", span, ident),
                }
            }
            Expression {
                span: _,
                inner: ExpressionEnum::Eq(lhs, rhs),
            } => {
                if lhs.eval(stack_frame)? == rhs.eval(stack_frame)? {
                    Value::Boolean(true)
                } else {
                    Value::Boolean(false)
                }
            }
            Expression {
                span: _,
                inner: ExpressionEnum::Gt(lhs, rhs),
            } => {
                if lhs.eval(stack_frame)? > rhs.eval(stack_frame)? {
                    Value::Boolean(true)
                } else {
                    Value::Boolean(false)
                }
            }
            Expression {
                span: _,
                inner: ExpressionEnum::Lt(lhs, rhs),
            } => {
                if lhs.eval(stack_frame)? < rhs.eval(stack_frame)? {
                    Value::Boolean(true)
                } else {
                    Value::Boolean(false)
                }
            }
            Expression {
                span: _,
                inner: ExpressionEnum::Ge(lhs, rhs),
            } => {
                if lhs.eval(stack_frame)? >= rhs.eval(stack_frame)? {
                    Value::Boolean(true)
                } else {
                    Value::Boolean(false)
                }
            }
            Expression {
                span: _,
                inner: ExpressionEnum::Le(lhs, rhs),
            } => {
                if lhs.eval(stack_frame)? <= rhs.eval(stack_frame)? {
                    Value::Boolean(true)
                } else {
                    Value::Boolean(false)
                }
            }
        };
        EvalResult::Continue(result)
    }

    pub(crate) fn type_check(
        &'src self,
        context: &mut TypeCheckContext<'src, '_>,
    ) -> Result<TypeDeclare, TypeCheckError<'src>> {
        Ok(match &self {
            Expression {
                span,
                inner: ExpressionEnum::Ident(ident),
            } => context
                .get_variable(ident)
                .ok_or(TypeCheckError::UndefinedVariable(*span, ident.to_string()))?,
            Expression {
                span: _,
                inner: ExpressionEnum::Value(Value::Boolean(_)),
            } => TypeDeclare::Boolean,
            Expression {
                span: _,
                inner: ExpressionEnum::Value(Value::F64(_)),
            } => TypeDeclare::F64,
            Expression {
                span: _,
                inner: ExpressionEnum::Value(Value::I64(_)),
            } => TypeDeclare::I64,
            Expression {
                span: _,
                inner: ExpressionEnum::Value(Value::String(_)),
            } => TypeDeclare::String,
            Expression {
                span: _,
                inner: ExpressionEnum::Value(Value::EmptyTuple),
            } => TypeDeclare::EmptyTuple,
            Expression {
                span,
                inner: ExpressionEnum::FnInvoke(ident, args),
            } => {
                let args_type = args
                    .iter()
                    .map(|v| Ok((v.type_check(context)?, v.span())))
                    .collect::<Result<Vec<_>, _>>()?;
                let func = context
                    .get_function(ident)
                    .ok_or(TypeCheckError::UndefinedFunction(*span, ident.to_string()))?;
                let params = func.params();
                for pair in args_type.iter().zip_longest(params.iter()) {
                    match pair {
                        itertools::EitherOrBoth::Both((arg_type, arg_span), (_param_name, param_type)) => {
                            arg_type.coerce_type(&param_type, *arg_span)? // TODO: Ident の中身に直接アクセスしないようにする
                        }
                        itertools::EitherOrBoth::Left((_, arg_span)) => {
                            return Err(TypeCheckError::InvalidArgumentCount(*arg_span));
                        }
                        itertools::EitherOrBoth::Right((_param_name, _param_type)) => {
                            return Err(TypeCheckError::InvalidArgumentCount(*span));
                            // TODO: param_name.0 への直接のアクセスを禁ずる
                        }
                    };
                }
                func.return_type()
            }
            Expression {
                span,
                inner: ExpressionEnum::If(condition, true_statements, false_statement),
            } => {
                let condition_type = condition.type_check(context)?;
                if condition_type != TypeDeclare::Boolean {
                    return Err(TypeCheckError::InvalidConditionType(*span));
                }
                let true_type = true_statements.type_check(context)?;
                if let Some(false_statements) = false_statement {
                    let false_type = false_statements.type_check(context)?;
                    true_type.coerce_type(&false_type, *span)?
                } else {
                    true_type
                }
            }
            Expression {
                span,
                inner: ExpressionEnum::Add(lhs, rhs),
            } => type_check_binary_op(lhs, rhs, context, BinaryOp::Add, *span)?,
            Expression {
                span,
                inner: ExpressionEnum::Sub(lhs, rhs),
            } => type_check_binary_op(lhs, rhs, context, BinaryOp::Sub, *span)?,
            Expression {
                span,
                inner: ExpressionEnum::Mul(lhs, rhs),
            } => type_check_binary_op(lhs, rhs, context, BinaryOp::Mul, *span)?,
            Expression {
                span,
                inner: ExpressionEnum::Div(lhs, rhs),
            } => type_check_binary_op(lhs, rhs, context, BinaryOp::Div, *span)?,
            Expression {
                span,
                inner: ExpressionEnum::Rem(lhs, rhs),
            } => type_check_binary_op(lhs, rhs, context, BinaryOp::Rem, *span)?,
            Expression {
                span,
                inner: ExpressionEnum::Eq(lhs, rhs),
            } => type_check_binary_op(lhs, rhs, context, BinaryOp::Eq, *span)?,
            Expression {
                span,
                inner: ExpressionEnum::Gt(lhs, rhs),
            } => type_check_binary_op(lhs, rhs, context, BinaryOp::Gt, *span)?,
            Expression {
                span,
                inner: ExpressionEnum::Lt(lhs, rhs),
            } => type_check_binary_op(lhs, rhs, context, BinaryOp::Lt, *span)?,
            Expression {
                span,
                inner: ExpressionEnum::Ge(lhs, rhs),
            } => type_check_binary_op(lhs, rhs, context, BinaryOp::Ge, *span)?,
            Expression {
                span,
                inner: ExpressionEnum::Le(lhs, rhs),
            } => type_check_binary_op(lhs, rhs, context, BinaryOp::Le, *span)?,
        })
    }
}

// 因子は数値リテラル または 識別子 または () で囲まれた式、関数呼び出し のいずれかである
fn factor<'src>(
    input: LocatedSpan<&'src str>,
) -> IResult<LocatedSpan<&'src str>, Expression<'src>> {
    alt((function_call, value, parens))(input)
}

// () で囲まれた式をパースする
fn parens<'src>(
    input: LocatedSpan<&'src str>,
) -> IResult<LocatedSpan<&'src str>, Expression<'src>> {
    delimited(
        multispace0,
        delimited(tag("("), expr, tag(")")),
        multispace0,
    )(input)
}

// 浮動小数点数をパースする
fn double_number<'src>(input: LocatedSpan<&'src str>) -> IResult<LocatedSpan<&'src str>, Value> {
    let (input, float) = delimited(multispace0, double, multispace0)(input)?;
    Ok((input, Value::F64(float)))
}

// 整数をパースする
fn int_number<'src>(input: LocatedSpan<&'src str>) -> IResult<LocatedSpan<&'src str>, Value> {
    let (input, int) = delimited(
        multispace0,
        recognize(preceded(opt(tag("-")), digit1)),
        multispace0,
    )(input)?;
    let int = int
        .to_string()
        .parse::<i64>()
        .expect(&format!("invalid integer: {int}"));
    Ok((input, Value::I64(int)))
}

// 数値式をパースする
fn number_expr<'src>(
    input: LocatedSpan<&'src str>,
) -> IResult<LocatedSpan<&'src str>, Expression<'src>> {
    let (rest_double, double) = double_number(input)?;
    let (rest_int, int) = int_number(input)?;
    let (rest, value) = if rest_double == rest_int {
        (rest_int, int)
    } else {
        (rest_double, double)
    };
    Ok((
        rest,
        Expression::new(
            helper::span_taken(input, rest),
            ExpressionEnum::Value(value),
        ),
    ))
}

// 文字列リテラルをパースする
fn string_literal<'src>(input: LocatedSpan<&'src str>) -> IResult<LocatedSpan<&'src str>, Value> {
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
fn string_expr<'src>(
    input: LocatedSpan<&'src str>,
) -> IResult<LocatedSpan<&'src str>, Expression<'src>> {
    let (rest, value) = string_literal(input)?;
    Ok((
        rest,
        Expression::new(
            helper::span_taken(input, rest),
            ExpressionEnum::Value(value),
        ),
    ))
}

// 真偽値をパースする
fn boolean<'src>(input: LocatedSpan<&'src str>) -> IResult<LocatedSpan<&'src str>, Value> {
    let (input, value) = alt((tag("true"), tag("false")))(input)?;
    let value = if *value.fragment() == "true" {
        Value::Boolean(true)
    } else if *value.fragment() == "false" {
        Value::Boolean(false)
    } else {
        unreachable!("invalid boolean literal: {value}")
    };
    Ok((input, value))
}

// 真偽値式をパースする
fn boolean_expr<'src>(
    input: LocatedSpan<&'src str>,
) -> IResult<LocatedSpan<&'src str>, Expression<'src>> {
    let (rest, value) = boolean(input)?;
    Ok((
        rest,
        Expression::new(
            helper::span_taken(input, rest),
            ExpressionEnum::Value(value),
        ),
    ))
}

// 識別子をパースする
pub(crate) fn ident<'src>(
    input: LocatedSpan<&'src str>,
) -> IResult<LocatedSpan<&'src str>, Ident<'src>> {
    let (input, ident) = delimited(
        multispace0,
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_")))),
        )),
        multispace0,
    )(input)?;
    Ok((input, Ident(ident.fragment())))
}

// 識別子式をパースする
fn ident_expr<'src>(
    input: LocatedSpan<&'src str>,
) -> IResult<LocatedSpan<&'src str>, Expression<'src>> {
    let (rest, ident) = ident(input)?;
    Ok((
        rest,
        Expression::new(
            helper::span_taken(input, rest),
            ExpressionEnum::Ident(ident),
        ),
    ))
}

// 値をパースする
fn value<'src>(input: LocatedSpan<&'src str>) -> IResult<LocatedSpan<&'src str>, Expression<'src>> {
    let (input, value) = alt((number_expr, string_expr, boolean_expr, ident_expr))(input)?;
    Ok((input, value))
}

pub fn expr<'src>(
    input: LocatedSpan<&'src str>,
) -> IResult<LocatedSpan<&'src str>, Expression<'src>> {
    alt((if_expr, comparison_expr, num_expr))(input)
}

fn if_expr<'src>(
    input: LocatedSpan<&'src str>,
) -> IResult<LocatedSpan<&'src str>, Expression<'src>> {
    let (rest, _) = delimited(multispace0, tag("if"), multispace1)(input)?;
    let (rest, cond) = expr(rest)?;
    let (rest, _) = helper::open_brace(rest)?;
    let (rest, then) = Statements::parse(rest)?;
    let (rest, _) = helper::close_brace(rest)?;
    let (rest, otherwise) = opt(preceded(
        delimited(multispace0, tag("else"), multispace0),
        delimited(helper::open_brace, Statements::parse, helper::close_brace),
    ))(rest)?;
    Ok((
        rest,
        Expression::new(
            helper::span_taken(input, rest),
            ExpressionEnum::If(Box::new(cond), Box::new(then), otherwise.map(Box::new)),
        ),
    ))
}

fn comparison_expr<'src>(
    input: LocatedSpan<&'src str>,
) -> IResult<LocatedSpan<&'src str>, Expression<'src>> {
    let (rest, lhs) = alt((if_expr, num_expr))(input)?;
    let (rest, op) = alt((tag(">="), tag("<="), tag(">"), tag("<"), tag("==")))(rest)?;
    let (rest, rhs) = alt((if_expr, num_expr))(rest)?;
    Ok((
        rest,
        Expression::new(
            helper::span_taken(input, rest),
            match *op.fragment() {
                ">" => ExpressionEnum::Gt(Box::new(lhs), Box::new(rhs)),
                "<" => ExpressionEnum::Lt(Box::new(lhs), Box::new(rhs)),
                ">=" => ExpressionEnum::Ge(Box::new(lhs), Box::new(rhs)),
                "<=" => ExpressionEnum::Le(Box::new(lhs), Box::new(rhs)),
                "==" => ExpressionEnum::Eq(Box::new(lhs), Box::new(rhs)),
                _ => unreachable!("invalid comparison operator"),
            },
        ),
    ))
}

// 式（加算式）をパースする
fn num_expr<'src>(
    input: LocatedSpan<&'src str>,
) -> IResult<LocatedSpan<&'src str>, Expression<'src>> {
    let (rest, lhs) = term(input)?;

    fold_many0(
        pair(
            delimited(multispace0, alt((tag("+"), tag("-"))), multispace0),
            term,
        ),
        move || lhs.clone(),
        |lhs, (op, rhs)| {
            let span = helper::span_taken(input, lhs.located_span());
            match *op.fragment() {
                "+" => Expression::new(span, ExpressionEnum::Add(Box::new(lhs), Box::new(rhs))),
                "-" => Expression::new(span, ExpressionEnum::Sub(Box::new(lhs), Box::new(rhs))),
                _ => unreachable!("Multiplicative operator is not allowed in additive expression"),
            }
        },
    )(rest)
}

// 項（乗算式）をパースする
fn term<'src>(input: LocatedSpan<&'src str>) -> IResult<LocatedSpan<&'src str>, Expression<'src>> {
    let (rest, lhs) = factor(input)?;

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
        |lhs, (op, rhs)| {
            let span = helper::span_taken(input, lhs.located_span());
            match *op.fragment() {
                "*" => Expression::new(span, ExpressionEnum::Mul(Box::new(lhs), Box::new(rhs))),
                "/" => Expression::new(span, ExpressionEnum::Div(Box::new(lhs), Box::new(rhs))),
                "%" => Expression::new(span, ExpressionEnum::Rem(Box::new(lhs), Box::new(rhs))),
                // "^" => Expression::Pow(Box::new(lhs), Box::new(rhs)),
                _ => unreachable!("invalid multiplicative operator"),
            }
        },
    )(rest)
}

// 関数呼び出しをパースする
fn function_call<'src>(
    input: LocatedSpan<&'src str>,
) -> IResult<LocatedSpan<&'src str>, Expression<'src>> {
    let (rest, ident) = ident(input)?;

    let (rest, args) = delimited(
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
    )(rest)?;

    Ok((
        rest,
        Expression::new(
            helper::span_taken(input, rest),
            ExpressionEnum::FnInvoke(ident, args),
        ),
    ))
}
