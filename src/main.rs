use std::io;

mod parser;
mod prover;

fn main() {
    loop {
        println!("Enter formula or q to quit:");

        let mut expression = String::new();
        io::stdin()
            .read_line(&mut expression)
            .expect("Failed to read line");
        let expression = expression.trim();

        if expression == "q" {
            break;
        }

        let expr = parser::parse_expression(expression);
        println!("Parsed expression: {:?}", expr);

        let (proven, proof_tree) = expr.prove();

        if proven {
            println!("Statement is valid. Proof tree: {:#?}", proof_tree);
        } else {
            println!("Not provable. Partial proof tree: {:#?}", proof_tree);
        }
    }
}
