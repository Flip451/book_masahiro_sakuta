use std::ops::ControlFlow;

use crate::{break_result::BreakResult, expression::Ident, stack_frame::StackFrame, statement::Statements};

pub(crate) enum FnDef<'src> {
    User(UserFn<'src>),
    Native(NativeFn),
}

impl<'src> FnDef<'src> {
    pub(crate) fn call(&self, args: &[f64], stack_frame: &StackFrame<'src>) -> f64 {
        match self {
            FnDef::User(f) => f.call(args, stack_frame),
            FnDef::Native(NativeFn(f)) => f(args),
        }
    }
}

pub(crate) struct UserFn<'src> {
    params: &'src [Ident<'src>],
    body: &'src Statements<'src>,
}

impl<'src> UserFn<'src> {
    pub(crate) fn new(params: &'src [Ident<'src>], body: &'src Statements<'src>) -> Self {
        Self { params, body }
    }

    fn call(&self, args: &[f64], stack_frame: &StackFrame<'src>) -> f64 {
        let mut new_stack_frame = StackFrame::push(stack_frame);

        for (ident, arg) in self.params.iter().zip(args.iter()) {
            new_stack_frame.insert_variable(*ident, *arg);
        }
        match self.body.eval(&mut new_stack_frame) {
            ControlFlow::Continue(n) => n,
            ControlFlow::Break(BreakResult::Return(n)) => n,
            ControlFlow::Break(BreakResult::Break) => {
                panic!("Breaking outside loop is prohibited");
            }
            ControlFlow::Break(BreakResult::Continue) => {
                panic!("Continuing outside loop is prohibited");
            }
        }
    }
}

pub(crate) struct NativeFn(Box<dyn Fn(&[f64]) -> f64>);

// 単項関数を式の配列に対する関数に変換する関数
pub(crate) fn unary_fn<'src>(f: impl Fn(f64) -> f64 + 'static) -> FnDef<'src> {
    FnDef::Native(NativeFn(Box::new(move |args| {
        let arg = args.first().expect("function missing argument");
        f(*arg)
    })))
}

// 二項関数を式の配列に対する関数に変換する関数
pub(crate) fn binary_fn<'src>(f: impl Fn(f64, f64) -> f64 + 'static) -> FnDef<'src> {
    FnDef::Native(NativeFn(Box::new(move |args| {
        let first_arg = args.first().expect("function missing the first argument");
        let second_arg = args.get(1).expect("function missing the second argument");
        f(*first_arg, *second_arg)
    })))
}

pub(crate) fn print(args: f64) -> f64 {
    println!("print: {:?}", args);
    0.0
}
