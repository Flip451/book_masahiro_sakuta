use std::ops::ControlFlow;

#[derive(PartialEq)]
pub enum BreakResult {
    Return(f64),
    Break,
    Continue,
}

pub(crate) type EvalResult = ControlFlow<BreakResult, f64>;
