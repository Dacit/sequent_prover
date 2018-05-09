use itertools::Itertools;

use super::prover::ProofNode;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Formula {
    True,
    False,
    Atom(String),
    Not(Box<Formula>),
    Or(Box<Formula>, Box<Formula>),
    And(Box<Formula>, Box<Formula>),
    Implication(Box<Formula>, Box<Formula>),
}

impl Formula {
    pub fn to_string(&self) -> String {
        match self {
            &Formula::True => String::from("1"),
            &Formula::False => String::from("0"),
            &Formula::Atom(ref s) => s.clone(),
            &Formula::Not(ref f) => "-".to_string() + &f.as_ref().to_string(),
            &Formula::And(ref f1, ref f2) => {
                f1.as_ref().to_string() + " & " + &f2.as_ref().to_string()
            }
            &Formula::Or(ref f1, ref f2) => {
                f1.as_ref().to_string() + " | " + &f2.as_ref().to_string()
            }
            &Formula::Implication(ref f1, ref f2) => {
                f1.as_ref().to_string() + " > " + &f2.as_ref().to_string()
            }
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Expression {
    pub l: Vec<Formula>,
    pub r: Vec<Formula>,
}

impl Expression {
    pub fn to_string(&self) -> String {
        self.l
            .iter()
            .map(Formula::to_string)
            .fold1(|f, g| f + &", " + &g)
            .unwrap_or("".to_string()) + &" => "
            + &self.r
                .iter()
                .map(Formula::to_string)
                .fold1(|f, g| f + &", " + &g)
                .unwrap_or("".to_string())
    }

    pub fn prove(&self) -> (bool, ProofNode) {
        let mut root = ProofNode::new(self.clone());

        (root.prove(), root)
    }
}

/// Parse sequent formula in form "F1, ..., Fn => G1, ..., Gm" into an expression
pub fn parse_expression(input: &str) -> Expression {
    let mut split = input.trim().split("=>");

    let expr = Expression {
        l: parse_formulas(split.next().expect("Not in sequent from")),
        r: parse_formulas(split.next().expect("Not in sequent form")),
    };
    assert!(split.next().is_none(), "Not in sequent form");
    expr
}

/// Parse "F1, ..., Fn" into a vector of formulas
fn parse_formulas(input: &str) -> Vec<Formula> {
    if input.is_empty() {
        vec![]
    } else {
        input.split(",").map(parse_formula).collect()
    }
}

fn split_expr(x: &str, pos: usize, op: &Fn(Box<Formula>, Box<Formula>) -> Formula) -> Formula {
    op(
        Box::new(parse_formula(&x[..pos])),
        Box::new(parse_formula(&x[pos + 1..])),
    )
}

/// Parse formula as defined in lecture into tree structure. CFG:
/// F = 0 | 1 | -F | F&F | F|F | F>F | S
/// S from {chars} \ {|&->,}
fn parse_formula(input: &str) -> Formula {
    let input = input.trim();

    match (input.find('>'), input.find('|'), input.find('&')) {
        (Some(pos), _, _) => split_expr(&input, pos, &Formula::Implication),
        (_, Some(pos), _) => split_expr(&input, pos, &Formula::Or),
        (_, _, Some(pos)) => split_expr(&input, pos, &Formula::And),
        _ => match input {
            "0" => Formula::False,
            "1" => Formula::True,
            x if x.starts_with('-') => Formula::Not(Box::new(parse_formula(&x[1..]))),
            x => Formula::Atom(String::from(x)),
        },
    }
}

#[cfg(test)]
mod tests {
    use parser::Formula::*;
    use parser::*;

    #[test]
    #[should_panic]
    fn test_empty_str() {
        parse_expression(" ");
    }

    #[test]
    fn test_empty_sets() {
        let res = parse_expression("=>");
        assert_eq!(
            Expression {
                l: vec![],
                r: vec![],
            },
            res
        );
    }

    #[test]
    fn test_complex_expr() {
        let expr_str = "0 | -B & C >1 =>D > F> G";
        let expr = Expression {
            l: vec![Implication(
                Box::from(Or(
                    Box::from(False),
                    Box::from(And(
                        Box::from(Not(Box::from(Atom(String::from("B"))))),
                        Box::from(Atom(String::from("C"))),
                    )),
                )),
                Box::from(True),
            )],
            r: vec![Implication(
                Box::from(Atom(String::from("D"))),
                Box::from(Implication(
                    Box::from(Atom(String::from("F"))),
                    Box::from(Atom(String::from("G"))),
                )),
            )],
        };

        let res = parse_expression(expr_str);
        assert_eq!(expr, res);
    }
}
