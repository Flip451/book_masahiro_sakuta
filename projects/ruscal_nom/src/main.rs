use std::{
    error::Error,
    io::{self, Read},
};

use ruscal_nom::{stack_frame::StackFrame, statement::Statements};

fn main() -> Result<(), Box<dyn Error>> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;

    let mut stack_frame = StackFrame::new();

    match Statements::parse(&input) {
        Ok((_, statements)) => {
            statements.eval(&mut stack_frame);
        }
        Err(e) => {
            eprintln!("Error: {}", e);
        }
    }
    Ok(())
}
