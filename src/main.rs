mod parser;
mod prover;
mod dt;

fn main() {
    let formula = "F & G | C > D => G, -H";
    let expr = parser::parse_expression(&formula);
    println!("{:?}", &formula);

    prover::proof(&expr);
}
