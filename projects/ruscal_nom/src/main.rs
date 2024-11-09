use std::{
    error::Error,
    io::{self, Read},
};

use ruscal_nom::{stack_frame::StackFrame, statement::Statements, type_check::TypeCheckContext};

fn main() -> Result<(), Box<dyn Error>> {
    let mut source = String::new();
    io::stdin().read_to_string(&mut source)?;

    let statements = match Statements::parse(&source) {
        Ok((_, statements)) => statements,
        Err(e) => {
            eprintln!("parse error: {}", e);
            return Ok(());
        }
    };

    println!("AST: {statements:#?}");

    let mut context = TypeCheckContext::new();
    if let Err(err) = statements.type_check(&mut context) {
        eprintln!("type check error: {}", err);
        return Ok(());
    }

    println!("Type check succeeded!");

    let mut stack_frame = StackFrame::new();
    statements.eval(&mut stack_frame);

    Ok(())
}
