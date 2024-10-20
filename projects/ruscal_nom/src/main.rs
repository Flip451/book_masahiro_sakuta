use std::{
    collections::HashMap,
    error::Error,
    io::{self, Read},
};

use ruscal_nom::statement::Statements;

fn main() -> Result<(), Box<dyn Error>> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;

    let mut variables = HashMap::new();

    match Statements::parse(&input) {
        Ok((_, statements)) => {
            statements.eval(&mut variables)?;
        }
        Err(e) => {
            eprintln!("Error: {}", e);
        }
    }
    Ok(())
}
