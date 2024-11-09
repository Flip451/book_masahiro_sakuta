use std::collections::HashMap;

use crate::{expression::Ident, type_check::TypeDeclare, value::Value};

pub(crate) fn standard_constants() -> HashMap<Ident<'static>, Value> {
    let mut constants = HashMap::new();
    constants.insert(Ident("pi"), Value::F64(std::f64::consts::PI));
    constants
}

pub(crate) fn standard_constants_types() -> HashMap<Ident<'static>, TypeDeclare> {
    let mut constants = HashMap::new();
    constants.insert(Ident("pi"), TypeDeclare::F64);
    constants
}
