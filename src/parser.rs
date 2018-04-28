#[derive(Debug)]
pub enum Formula {
    True,
    False,
    Atom(String),
    Not(Box<Formula>),
    Or(Box<Formula>, Box<Formula>),
    And(Box<Formula>, Box<Formula>),
    Implication(Box<Formula>, Box<Formula>),
}

#[derive(Debug)]
pub struct Expression {
    pub l: Vec<Formula>,
    pub r: Vec<Formula>,
}

/// Parse sequent formula in form "F1, ..., Fn => G1, ..., Gm" into an expression
pub fn parse_expression(input: &str) -> Expression {
    let split: Vec<&str> = input.split("=>").collect();

    if split.len() != 2 {
        panic!("Not in sequent form");
    } else {
        Expression { l: parse_formulas(split[0]), r: parse_formulas(split[1]) }
    }
}

/// Parse "F1, ..., Fn" into a vector of formulas
fn parse_formulas(input: &str) -> Vec<Formula> {
    input.split(",").map(parse_formula).collect()
}

fn split_expr(x: &str, pos: usize, op: &Fn(Box<Formula>, Box<Formula>) -> Formula) -> Formula {
    op(Box::new(parse_formula(&x[..pos])), Box::new(parse_formula(&x[pos + 1..])))
}

/// Parse formula as defined in lecture into tree structure. CFG:
/// F = 0 | 1 | -F | F&F | F|F | F>F | S
/// S from {chars} \ {|&->,}
fn parse_formula(input: &str) -> Formula {
    let trimmed_f = input.trim_matches(' ');

    match (trimmed_f.find('>'), trimmed_f.find('|'), trimmed_f.find('&')) {
        (Some(pos), _, _) => split_expr(&trimmed_f, pos, &Formula::Implication),
        (_, Some(pos), _) => split_expr(&trimmed_f, pos, &Formula::Or),
        (_, _, Some(pos)) => split_expr(&trimmed_f, pos, &Formula::And),
        _ => match trimmed_f {
            "0" => Formula::False,
            "1" => Formula::True,
            x if x.starts_with('-') => {
                Formula::Not(Box::new(parse_formula(&x[1..])))
            }
            x => Formula::Atom(String::from(x)),
        },
    }
}