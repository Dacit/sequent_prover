mod parser;

fn main() {
    let formula = "F & G | C > D => G, -H";

    println!("{:?}", parser::parse_expression(formula));
}
