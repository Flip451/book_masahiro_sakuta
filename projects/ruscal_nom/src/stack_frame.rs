use std::collections::HashMap;

use crate::{
    constants, expression::Ident, function::{self, FnDef}, value::Value
};

#[derive(Default)]
pub struct StackFrame<'src> {
    variables: HashMap<Ident<'src>, Value>,
    functions: HashMap<Ident<'src>, FnDef<'src>>,
    parent: Option<&'src StackFrame<'src>>,
}

impl<'src> StackFrame<'src> {
    pub fn new() -> Self {
        Self {
            variables: constants::standard_constants(),
            functions: function::standard_functions(),
            parent: None,
        }
    }

    pub(crate) fn push(parent: &'src Self) -> Self {
        Self {
            variables: HashMap::new(),
            functions: HashMap::new(),
            parent: Some(parent),
        }
    }

    pub(crate) fn insert_variable(&mut self, ident: Ident<'src>, value: Value) {
        self.variables.insert(ident, value);
    }

    pub(crate) fn insert_function(&mut self, ident: Ident<'src>, function: FnDef<'src>) {
        self.functions.insert(ident, function);
    }

    pub(crate) fn get_variable(&self, ident: Ident<'src>) -> Option<Value> {
        self.variables.get(&ident).cloned()
    }

    pub(crate) fn get_function(&self, ident: Ident<'src>) -> Option<&FnDef<'src>> {
        match self.functions.get(&ident) {
            Some(function) => Some(function),
            None => match self.parent {
                Some(parent) => parent.get_function(ident),
                None => None,
            },
        }
    }
}
