use std::collections::HashMap;

use nom::{
    branch::alt, bytes::complete::tag, character::complete::multispace0, sequence::delimited,
    IResult, Parser,
};
use nom_locate::LocatedSpan;
use thiserror::Error;

use crate::{
    constants,
    expression::{Expression, Ident},
    function::{self, FnDef},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Span<'src>(pub(crate) LocatedSpan<&'src str>);

impl<'src> Span<'src> {
    pub(crate) fn new(input: LocatedSpan<&'src str>) -> Self {
        Self(input)
    }

    pub(crate) fn into_inner(self) -> LocatedSpan<&'src str> {
        self.0
    }
}

impl<'src> std::fmt::Display for Span<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "location: {}:{}",
            self.0.location_line(),
            self.0.get_utf8_column()
        )
    }
}

#[derive(Debug, Error)]
pub enum TypeCheckError<'src> {
    #[error("{0}: type mismatch. {1} cannot be assigned to {2}")]
    TypeMismatch(Span<'src>, TypeDeclare, TypeDeclare),
    #[error("{0}: undefined variable: {1:?}")]
    UndefinedVariable(Span<'src>, Ident<'src>),
    #[error("{0}: undefined function: {1:?}")]
    UndefinedFunction(Span<'src>, Ident<'src>),
    #[error("{0}: invalid binary operation: {1:?} is not defined between {2:?} and {3:?}")]
    InvalidBinaryOperation(Span<'src>, BinaryOp, TypeDeclare, TypeDeclare),
    #[error("{0}: invalid argument count")]
    InvalidArgumentCount(Span<'src>),
    #[error("{0}: invalid condition type")]
    InvalidConditionType(Span<'src>),
}

pub struct TypeCheckContext<'src, 'ctx> {
    variables: HashMap<Ident<'src>, TypeDeclare>,
    functions: HashMap<Ident<'src>, FnDef<'src>>,
    parent: Option<&'ctx TypeCheckContext<'src, 'ctx>>,
}

impl<'src, 'ctx> TypeCheckContext<'src, 'ctx> {
    pub fn new() -> Self {
        Self {
            variables: constants::standard_constants_types(),
            functions: function::standard_functions(),
            parent: None,
        }
    }

    pub(crate) fn push(context: &'ctx Self) -> TypeCheckContext<'src, 'ctx> {
        Self {
            variables: HashMap::new(),
            functions: HashMap::new(),
            parent: Some(context),
        }
    }

    pub(crate) fn get_variable(&self, ident: &Ident<'src>) -> Option<TypeDeclare> {
        self.variables.get(ident).cloned()
    }

    pub(crate) fn get_function(&self, ident: &Ident<'src>) -> Option<&FnDef<'src>> {
        match self.functions.get(ident) {
            Some(func) => Some(func),
            None => match self.parent {
                Some(parent) => parent.get_function(ident),
                None => None,
            },
        }
    }

    pub(crate) fn insert_variable(&mut self, ident: Ident<'src>, type_declare: TypeDeclare) {
        self.variables.insert(ident, type_declare);
    }

    pub(crate) fn insert_function(&mut self, ident: Ident<'src>, function: FnDef<'src>) {
        self.functions.insert(ident, function);
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TypeDeclare {
    Any,
    I64,
    F64,
    String,
    Boolean,
    EmptyTuple,
}

impl std::fmt::Display for TypeDeclare {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl TypeDeclare {
    pub(crate) fn coerce_type<'src>(
        &self,
        target: &TypeDeclare,
        span: Span<'src>,
    ) -> Result<TypeDeclare, TypeCheckError<'src>> {
        use TypeDeclare::*;
        Ok(match (self, target) {
            (Any, _) => *target,
            (_, Any) => *self,
            (I64, I64) => I64,
            (I64, F64) => F64,
            (F64, I64) => F64,
            (F64, F64) => F64,
            (String, String) => String,
            (Boolean, Boolean) => Boolean,
            (EmptyTuple, EmptyTuple) => EmptyTuple,
            _ => return Err(TypeCheckError::TypeMismatch(span, *self, *target)),
        })
    }

    pub(crate) fn parse(input: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, Self> {
        use TypeDeclare::*;
        let (input, type_declare) = delimited(
            multispace0,
            alt((
                tag("i64").map(|_| I64),
                tag("f64").map(|_| F64),
                tag("string").map(|_| String),
                tag("boolean").map(|_| Boolean),
                tag("bool").map(|_| Boolean),
                tag("()").map(|_| EmptyTuple),
            )), // TODO: カスタム型への対応
            multispace0,
        )(input)?;
        Ok((input, type_declare))
    }
}

pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Eq,
    Gt,
    Lt,
    Ge,
    Le,
}

impl std::fmt::Debug for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use BinaryOp::*;
        write!(
            f,
            "{:?}",
            match self {
                Add => "+",
                Sub => "-",
                Mul => "*",
                Div => "/",
                Rem => "%",
                Eq => "==",
                Gt => ">",
                Lt => "<",
                Ge => ">=",
                Le => "<=",
            }
        )
    }
}

pub(crate) fn type_check_binary_op<'src, 'ctx>(
    lhs: &'src Expression<'src>,
    rhs: &'src Expression<'src>,
    context: &mut TypeCheckContext<'src, 'ctx>,
    op: BinaryOp,
    span: Span<'src>,
) -> Result<TypeDeclare, TypeCheckError<'src>> {
    use BinaryOp::*;
    use TypeDeclare::*;
    let lhs_type = lhs.type_check(context)?;
    let rhs_type = rhs.type_check(context)?;

    match (op, lhs_type, rhs_type) {
        // 四則演算
        (Add | Sub | Mul | Div | Rem, Any, _) => Ok(Any),
        (Add | Sub | Mul | Div | Rem, _, Any) => Ok(Any),
        (Add | Sub | Mul | Div | Rem, I64, I64) => Ok(I64),
        (Add | Sub | Mul | Div | Rem, I64, F64)
        | (Add | Sub | Mul | Div | Rem, F64, I64)
        | (Add | Sub | Mul | Div | Rem, F64, F64) => Ok(F64),
        (Add, String, String) => Ok(String),
        (op @ (Sub | Mul | Div | Rem), String, String) => Err(
            TypeCheckError::InvalidBinaryOperation(span, op, String, String),
        ),
        (op @ (Add | Sub | Mul | Div | Rem), lhs, rhs) => {
            Err(TypeCheckError::InvalidBinaryOperation(span, op, lhs, rhs))
        }
        // 比較演算
        (Eq | Gt | Lt | Ge | Le, Any, Any) => Ok(Boolean),
        (Eq | Gt | Lt | Ge | Le, Any, I64 | F64 | String) => Ok(Boolean),
        (Eq | Gt | Lt | Ge | Le, I64 | F64 | String, Any) => Ok(Boolean),
        (Eq | Gt | Lt | Ge | Le, I64 | F64, I64 | F64) => Ok(Boolean),
        (Eq | Gt | Lt | Ge | Le, String, String) => Ok(Boolean),
        (op @ (Eq | Gt | Lt | Ge | Le), lhs, rhs) => {
            Err(TypeCheckError::InvalidBinaryOperation(span, op, lhs, rhs))
        }
    }
}
