mod parser;
mod prover;

fn main() {
    let formula = "F, G>H => F";
    let expr = parser::parse_expression(&formula);
    println!("{:?}", &formula);

    println!("{:?}", prover::proof(&expr));
}
