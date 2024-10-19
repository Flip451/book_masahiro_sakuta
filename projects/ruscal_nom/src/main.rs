use std::{
    error::Error,
    io::{self, Read},
};

use ruscal_nom::statement;

fn main() -> Result<(), Box<dyn Error>> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;
    match statement::statements(&input) {
        Ok(statements) => {
            for statement in statements {
                println!("{:?}", statement.eval());
            }
        }
        Err(e) => {
            eprintln!("Error: {}", e);
        }
    }
    Ok(())
}
