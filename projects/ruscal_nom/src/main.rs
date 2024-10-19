use std::{
    collections::HashMap, error::Error, io::{self, Read}
};

use ruscal_nom::statement::{self, Statement};

fn main() -> Result<(), Box<dyn Error>> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;

    let mut variables = HashMap::new();

    match statement::statements(&input) {
        Ok(statements) => {
            for statement in statements {
                match statement {
                    Statement::LetDef(name, expr) => {
                        println!("{:?} = {:?}", name, expr);
                        variables.insert(name, expr.eval(&variables));
                    }
                    Statement::Expression(expr) => {
                        println!("{}", expr.eval(&variables));
                    }
                }
            }
        }
        Err(e) => {
            eprintln!("Error: {}", e);
        }
    }
    Ok(())
}
