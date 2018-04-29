use std::clone::Clone;
use super::parser::{Expression, Formula};

#[derive(Debug)]
pub enum Rule {
    Ax,
    LNot,
    LAnd,
    LOr,
    LImpl,
    RNot,
    RAnd,
    ROr,
    RImpl,
}

#[derive(Debug)]
pub struct ProofNode {
    pub expr: Box<Expression>,
    pub rule: Option<Rule>,
    pub proof_by: Option<Vec<Box<ProofNode>>>,
}

impl ProofNode {
    pub fn new(e: Box<Expression>) -> ProofNode {
        ProofNode { expr: e, rule: None, proof_by: None }
    }

    pub fn proof(&mut self) -> bool {
        let rules = [ProofNode::ax_rule, ProofNode::l_not_rule];//, ProofNode::l_and_rule, ProofNode::r_not_rule, ProofNode::r_or_rule, ProofNode::r_impl_rule, ProofNode::l_or_rule, ProofNode::l_impl_rule, ProofNode::r_and_rule];

        for rule in rules.iter() {
            if rule(self) {
                return (&mut self.proof_by.as_mut().unwrap()).iter_mut().map(|x|(*x).proof()).fold(true, |f,g|f && g )
            }
        }

        false
    }

    pub fn ax_rule(&mut self) -> bool {
        let left_atoms: Vec<String> = self.expr.l.iter()
            .filter_map(|f| match f {
                &Formula::Atom(ref a) => Some(a.clone()),
                _ => None
            }).collect();

        if self.expr.r.iter().any(|f| match f {
            &Formula::Atom(ref b) if left_atoms.contains(b) => true,
            _ => false
        }) {
            self.rule = Some(Rule::Ax);
            self.proof_by = Some(vec![]);
            true
        } else {
            false
        }
    }

    pub fn l_not_rule(&mut self) -> bool {
        match self.expr.l.iter().enumerate().find(|&(i, f)| match f {
            &Formula::Not(_) => true,
            _ => false
        }) {
            Some((i, &Formula::Not(ref g))) => {
                let mut expr = self.expr.clone();
                expr.l.remove(i);
                expr.r.insert(0, *g.clone());

                self.rule = Some(Rule::LNot);
                self.proof_by = Some(vec![Box::from(ProofNode::new(expr))]);
                return true
            },
            _ => false,
        }
    }
    pub fn l_and_rule(&mut self) -> bool {
        unimplemented!()
    }
    pub fn l_or_rule(&mut self) -> bool {
        unimplemented!()
    }
    pub fn l_impl_rule(&mut self) -> bool {
        unimplemented!()
    }
    pub fn r_not_rule(&mut self) -> bool { unimplemented!() }
    pub fn r_and_rule(&mut self) -> bool {
        unimplemented!()
    }
    pub fn r_or_rule(&mut self) -> bool {
        unimplemented!()
    }
    pub fn r_impl_rule(&mut self) -> bool {
        unimplemented!()
    }
}