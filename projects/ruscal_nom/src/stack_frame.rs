use std::collections::HashMap;

use crate::{
    expression::Ident,
    function::{self, FnDef},
};

#[derive(Default)]
pub struct StackFrame<'src> {
    variables: HashMap<Ident<'src>, f64>,
    functions: HashMap<Ident<'src>, FnDef<'src>>,
    parent: Option<&'src StackFrame<'src>>,
}

impl<'src> StackFrame<'src> {
    pub fn new() -> Self {
        let mut variables = HashMap::new();
        variables.insert(Ident("pi"), std::f64::consts::PI);

        let mut functions = HashMap::new();
        functions.insert(Ident("print"), function::unary_fn(function::print));
        functions.insert(Ident("sqrt"), function::unary_fn(f64::sqrt));
        functions.insert(Ident("sin"), function::unary_fn(f64::sin));
        functions.insert(Ident("cos"), function::unary_fn(f64::cos));
        functions.insert(Ident("tan"), function::unary_fn(f64::tan));
        functions.insert(Ident("asin"), function::unary_fn(f64::asin));
        functions.insert(Ident("acos"), function::unary_fn(f64::acos));
        functions.insert(Ident("atan"), function::unary_fn(f64::atan));
        functions.insert(Ident("atan2"), function::binary_fn(f64::atan2));
        functions.insert(Ident("pow"), function::binary_fn(f64::powf));
        functions.insert(Ident("exp"), function::unary_fn(f64::exp));
        functions.insert(Ident("ln"), function::unary_fn(f64::ln));
        functions.insert(Ident("log10"), function::unary_fn(f64::log10));
        functions.insert(Ident("log2"), function::unary_fn(f64::log2));
        functions.insert(Ident("log"), function::binary_fn(f64::log));

        Self {
            variables,
            functions,
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

    pub(crate) fn insert_variable(&mut self, ident: Ident<'src>, value: f64) {
        self.variables.insert(ident, value);
    }

    pub(crate) fn insert_function(&mut self, ident: Ident<'src>, function: FnDef<'src>) {
        self.functions.insert(ident, function);
    }

    pub fn get_variable(&self, ident: Ident<'src>) -> Option<f64> {
        match self.variables.get(&ident) {
            Some(&value) => Some(value),
            None => match self.parent {
                Some(parent) => parent.get_variable(ident),
                None => None,
            },
        }
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
