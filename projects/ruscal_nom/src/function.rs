use std::ops::ControlFlow;

use crate::{
    break_result::BreakResult, expression::Ident, stack_frame::StackFrame, statement::Statements, type_check::TypeDeclare, value::Value
};

pub(crate) struct NativeFn(pub(crate) Box<dyn Fn(&[Value]) -> Value>);

pub(crate) enum FnDef<'src> {
    User(UserFn<'src>),
    Native(NativeFn),
}

impl<'src> FnDef<'src> {
    pub(crate) fn call(&self, args: &[Value], stack_frame: &StackFrame<'src>) -> Value {
        match self {
            FnDef::User(f) => f.call(args, stack_frame),
            FnDef::Native(NativeFn(f)) => f(args),
        }
    }

    pub(crate) fn params(&self) -> &'src [(Ident<'src>, TypeDeclare)] {
        match self {
            FnDef::User(user_fn) => user_fn.params,
            FnDef::Native(native_fn) => todo!(),
        }
    }

    pub(crate) fn return_type(&self) -> TypeDeclare {
        match self {
            FnDef::User(user_fn) => user_fn.return_type,
            FnDef::Native(native_fn) => todo!(),
        }
    }
}

pub(crate) struct UserFn<'src> {
    params: &'src [(Ident<'src>, TypeDeclare)],
    return_type: TypeDeclare,
    body: &'src Statements<'src>,
}

impl<'src> UserFn<'src> {
    pub(crate) fn new(params: &'src [(Ident<'src>, TypeDeclare)], return_type: TypeDeclare, body: &'src Statements<'src>) -> Self {
        Self { params, return_type, body }
    }

    fn call(&self, args: &[Value], stack_frame: &StackFrame<'src>) -> Value {
        let mut new_stack_frame = StackFrame::push(stack_frame);

        for ((ident, _type_declare), arg) in self.params.iter().zip(args.iter()) {
            new_stack_frame.insert_variable(*ident, arg.clone());
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

// 単項関数を式の配列に対する関数に変換する関数
pub(crate) fn unary_fn<'src>(f: impl Fn(f64) -> f64 + 'static) -> FnDef<'src> {
    FnDef::Native(NativeFn(Box::new(move |args| {
        let arg = args.first().expect("function missing argument");
        Value::F64(f(arg.coerce_to_f64()))
    })))
}

// 二項関数を式の配列に対する関数に変換する関数
pub(crate) fn binary_fn<'src>(f: impl Fn(f64, f64) -> f64 + 'static) -> FnDef<'src> {
    FnDef::Native(NativeFn(Box::new(move |args| {
        let first_arg = args.first().expect("function missing the first argument");
        let second_arg = args.get(1).expect("function missing the second argument");
        Value::F64(f(first_arg.coerce_to_f64(), second_arg.coerce_to_f64()))
    })))
}

// 標準出力に値を出力する関数
fn print_raw(args: &[Value]) -> Value {
    println!("print: {}", args[0]);
    Value::EmptyTuple
}

pub(crate) fn print() -> FnDef<'static> {
    FnDef::Native(NativeFn(Box::new(print_raw)))
}

// 標準出力に値を出力する関数
fn print_dbg_raw(args: &[Value]) -> Value {
    println!("print_dbg: {:?}", args[0]);
    Value::EmptyTuple
}

pub(crate) fn print_dbg() -> FnDef<'static> {
    FnDef::Native(NativeFn(Box::new(print_dbg_raw)))
}

pub(crate) fn as_i64() -> FnDef<'static> {
    FnDef::Native(NativeFn(Box::new(|args| {
        let arg = args.first().expect("function missing argument");
        Value::I64(arg.as_i64().expect("value cannot be coerced to i64"))
    })))
}

pub(crate) fn as_f64() -> FnDef<'static> {
    FnDef::Native(NativeFn(Box::new(|args| {
        let arg = args.first().expect("function missing argument");
        Value::F64(arg.as_f64().expect("value cannot be coerced to f64"))
    })))
}

pub(crate) fn as_boolean() -> FnDef<'static> {
    FnDef::Native(NativeFn(Box::new(|args| {
        let arg = args.first().expect("function missing argument");
        Value::Boolean(arg.as_boolean().expect("value cannot be coerced to boolean"))
    })))
}

pub(crate) fn as_string() -> FnDef<'static> {
    FnDef::Native(NativeFn(Box::new(|args| {
        let arg = args.first().expect("function missing argument");
        Value::String(arg.as_string().expect("value cannot be coerced to string"))
    })))
}
