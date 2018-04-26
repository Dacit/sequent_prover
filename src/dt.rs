#[derive(Debug)]
pub enum Formula {
    True,
    False,
    Atom(Box<str>),
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