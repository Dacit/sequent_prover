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

#[derive(Debug)]
pub enum Rule {
    AX,
    LNOT,
    LAND,
    LOR,
    LIMPL,
    RNOT,
    RAND,
    ROR,
    RIMPL
}

#[derive(Debug)]
pub struct ProofNode<'a> {
    pub expr: &'a Expression,
    pub rule: Option<Rule>,
    pub proof_by: Option<Vec<ProofNode<'a>>>,
}

impl <'a>ProofNode<'a> {
    pub fn new(e: &Expression) -> ProofNode {
        ProofNode { expr: e, rule: None, proof_by: None }
    }
}