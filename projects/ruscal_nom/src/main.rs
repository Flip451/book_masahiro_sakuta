use std::{
    collections::{hash_map::Entry, HashMap}, error::Error, io::{self, Read}
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
                    Statement::VarDef(ident, expr) => {
                        println!("{:?} = {:?}", ident, expr);
                        variables.insert(ident, expr.eval(&variables));
                    }
                    Statement::Assignment(ident, expression) => {
                        println!("{:?} = {:?}", ident, expression);
                        if !variables.contains_key(&ident) {
                            panic!("Error: {:?} is not defined", ident);
                        }
                        variables.insert(ident, expression.eval(&variables));
                    },
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
