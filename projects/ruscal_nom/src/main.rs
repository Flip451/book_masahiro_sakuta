use anyhow::Result;
use nom_locate::LocatedSpan;
use std::io::{self, Read};

use ruscal_nom::{stack_frame::StackFrame, statement::Statements, type_check::TypeCheckContext};

fn main() -> Result<()> {
    let mut source = String::new();
    io::stdin().read_to_string(&mut source)?;

    let statements = match Statements::parse(LocatedSpan::new(&source)) {
        Ok((_, statements)) => statements,
        Err(e) => {
            eprintln!("parse error: {}", e);
            return Ok(());
        }
    };

    println!("AST: {statements:#?}");

    let mut context = TypeCheckContext::new();
    if let Err(err) = statements.type_check(&mut context) {
        eprintln!("Type check error: {}", err);
        return Ok(());
    }

    println!("Type check succeeded!");

    let mut stack_frame = StackFrame::new();
    statements.eval(&mut stack_frame);

    Ok(())
}
