use std::collections::HashMap;

use crate::{
    expression::Ident,
    function::{self, FnDef},
    value::Value,
};

#[derive(Default)]
pub struct StackFrame<'src> {
    variables: HashMap<Ident<'src>, Value>,
    functions: HashMap<Ident<'src>, FnDef<'src>>,
    parent: Option<&'src StackFrame<'src>>,
}

impl<'src> StackFrame<'src> {
    pub fn new() -> Self {
        let mut functions = HashMap::new();

        // 標準出力用の関数
        functions.insert(Ident("print"), function::print());
        functions.insert(Ident("dbg"), function::print_dbg());

        // 型キャスト用の関数
        functions.insert(Ident("i64"), function::as_i64());
        functions.insert(Ident("f64"), function::as_f64());
        functions.insert(Ident("bool"), function::as_boolean());
        functions.insert(Ident("str"), function::as_string());
        // 数学関数
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
            variables: HashMap::new(),
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
