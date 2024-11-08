use std::collections::HashMap;

use nom::{
    branch::alt, bytes::complete::tag, character::complete::multispace0, sequence::delimited,
    IResult, Parser,
};
use thiserror::Error;

use crate::{
    expression::{Expression, Ident},
    function::FnDef,
};

#[derive(Debug, Error)]
pub(crate) enum TypeCheckError<'src> {
    #[error("type mismatch. {0} cannot be assigned to {1}")]
    TypeMismatch(TypeDeclare, TypeDeclare),
    #[error("undefined variable: {0:?}")]
    UndefinedVariable(Ident<'src>),
    #[error("undefined function: {0:?}")]
    UndefinedFunction(Ident<'src>),
    #[error("invalid binary operation: {0:?} {1:?} {2:?}")]
    InvalidBinaryOperation(BinaryOp, TypeDeclare, TypeDeclare),
    #[error("invalid argument count")]
    InvalidArgumentCount,
}

pub(crate) struct TypeCheckContext<'src> {
    variables: HashMap<Ident<'src>, TypeDeclare>,
    functions: HashMap<Ident<'src>, FnDef<'src>>,
    parent: Option<&'src TypeCheckContext<'src>>,
}

impl<'src> TypeCheckContext<'src> {
    pub(crate) fn new() -> Self {
        Self {
            variables: HashMap::new(),
            functions: HashMap::new(),
            parent: None,
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
            _ => return Err(TypeCheckError::TypeMismatch(*self, *target)),
        })
    }

    pub(crate) fn parse(input: &str) -> IResult<&str, Self> {
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

pub(crate) enum BinaryOp {
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

pub(crate) fn type_check_binary_op<'src>(
    lhs: &Expression<'src>,
    rhs: &Expression<'src>,
    context: &mut TypeCheckContext<'src>,
    op: BinaryOp,
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
        (op @ (Sub | Mul | Div | Rem), String, String) => {
            Err(TypeCheckError::InvalidBinaryOperation(op, String, String))
        }
        (op @ (Add | Sub | Mul | Div | Rem), lhs, rhs) => {
            Err(TypeCheckError::InvalidBinaryOperation(op, lhs, rhs))
        }
        // 比較演算
        (Eq | Gt | Lt | Ge | Le, Any, Any) => Ok(Boolean),
        (Eq | Gt | Lt | Ge | Le, Any, I64 | F64 | String) => Ok(Boolean),
        (Eq | Gt | Lt | Ge | Le, I64 | F64 | String, Any) => Ok(Boolean),
        (Eq | Gt | Lt | Ge | Le, I64 | F64, I64 | F64) => Ok(Boolean),
        (Eq | Gt | Lt | Ge | Le, String, String) => Ok(Boolean),
        (op @ (Eq | Gt | Lt | Ge | Le), lhs, rhs) => {
            Err(TypeCheckError::InvalidBinaryOperation(op, lhs, rhs))
        }
    }
}
