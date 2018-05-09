extern crate itertools;

use std::io;
use std::panic::catch_unwind;

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

        let expr = catch_unwind(|| parser::parse_expression(expression)).ok();

        if expr.is_some() {
            println!("Parsed expression.");

            let (proven, proof_tree) = expr.unwrap().prove();

            if proven {
                println!("Statement is valid.");
                let (_, _, lines) = proof_tree.build_str();
                lines.iter().for_each(|s| println!("{}", s));
            } else {
                println!("Not provable. Partial proof tree: {:#?}", proof_tree);
            }
        }
    }
}
