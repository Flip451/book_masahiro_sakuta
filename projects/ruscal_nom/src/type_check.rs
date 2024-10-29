use std::collections::HashMap;

use nom::{
    branch::alt, bytes::complete::tag, character::complete::multispace0, sequence::delimited,
    IResult, Parser,
};
use thiserror::Error;

use crate::{expression::Ident, function::FnDef};

#[derive(Debug, Error)]
pub(crate) enum TypeCheckError {
    #[error("type mismatch. {0} cannot be assigned to {1}")]
    TypeMismatch(TypeDeclare, TypeDeclare),
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

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum TypeDeclare {
    Any,
    I64,
    F64,
    String,
    Boolean,
}

impl std::fmt::Display for TypeDeclare {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl TypeDeclare {
    fn coerce_type(&self, target: &TypeDeclare) -> Result<TypeDeclare, TypeCheckError> {
        use TypeDeclare::*;
        Ok(match (self, target) {
            (Any, _) => target.clone(),
            (_, Any) => self.clone(),
            (I64, I64) => I64,
            (I64, F64) => F64,
            (F64, I64) => I64,
            (F64, F64) => F64,
            (String, String) => String,
            (Boolean, Boolean) => Boolean,
            _ => return Err(TypeCheckError::TypeMismatch(self.clone(), target.clone())),
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
            )),  // TODO: カスタム型への対応
            multispace0,
        )(input)?;
        Ok((input, type_declare))
    }
}
