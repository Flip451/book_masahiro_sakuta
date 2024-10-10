use ruscal_nom::expression;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    let input = "123";
    println!(
        "source: {:?}, parsed: {:?}",
        input,
        expression::expr(input)?.1.eval()
    );

    let input = "2 * pi";
    println!(
        "source: {:?}, parsed: {:?}",
        input,
        expression::expr(input)?.1.eval()
    );

    let input = "(123 + 456 ) + pi";
    println!(
        "source: {:?}, parsed: {:?}",
        input,
        expression::expr(input)?.1.eval()
    );

    let input = "10 - (100 + 1)";
    println!(
        "source: {:?}, parsed: {:?}",
        input,
        expression::expr(input)?.1.eval()
    );

    let input = "(3 + 7) / (2 + 3)";
    println!(
        "source: {:?}, parsed: {:?}",
        input,
        expression::expr(input)?.1.eval()
    );

    let input = "(21.5 % 7) ^ 2";
    println!(
        "source: {:?}, parsed: {:?}",
        input,
        expression::expr(input)?.1.eval()
    );

    Ok(())
}
