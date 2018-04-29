use std::io;

mod parser;
mod prover;

fn main() {
    loop {
        println!("Enter formula (in form \"-A | B & C > D => 1\") or q to quit:");

        let mut expression = String::new();
        io::stdin().read_line(&mut expression).expect("Failed to read line");
        if expression.ends_with("\n") {
            expression.pop();
        }

        if expression == "q" {
            break;
        }

        let expr = parser::parse_expression(expression.as_str());
        println!("Parsed expression: {:?}", &expr);

        let mut root = prover::ProofNode::new(Box::from(expr));

        if root.proof() {
            println!("Statement is valid. Proof tree: {:?}", root);
        } else {
            println!("Not provable. Partial proof tree: {:?}", root);
        }
    }
}
