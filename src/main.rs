mod parser;
mod prover;

fn main() {
    let formula = "-F, F =>";
    let expr = parser::parse_expression(&formula);
    println!("{:?}", &formula);

    let mut root = prover::ProofNode::new(Box::from(expr));

    if root.proof() {
        println!("Statement is valid. Proof tree: {:?}", root);
    } else {
        println!("Not provable. Partial proof tree: {:?}", root);
    }
}
