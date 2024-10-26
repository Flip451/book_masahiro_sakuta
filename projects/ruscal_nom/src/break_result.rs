use std::ops::ControlFlow;

use crate::value::Value;

#[derive(PartialEq)]
pub enum BreakResult {
    Return(Value),
    Break,
    Continue,
}

pub(crate) type EvalResult = ControlFlow<BreakResult, Value>;
