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
        let rules = [ProofNode::ax_rule, ProofNode::l_not_rule, ProofNode::l_and_rule, ProofNode::r_not_rule, ProofNode::r_or_rule, ProofNode::r_impl_rule, ProofNode::l_or_rule, ProofNode::l_impl_rule, ProofNode::r_and_rule];

        for rule in rules.iter() {
            if rule(self) {
                return (&mut self.proof_by.as_mut().unwrap()).iter_mut()
                    .map(|x| (*x).proof()).fold(true, |f, g| f && g);
            }
        }

        false
    }

    fn ax_rule(&mut self) -> bool {
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

    fn find_formula(fs: &Vec<Formula>, rule: Rule) -> Option<(usize, &Formula)> {
        (*fs).iter().enumerate().find(|&(_, f)| match (f, &rule) {
            (&Formula::Not(_), &Rule::LNot) | (&Formula::Not(_), &Rule::RNot) => true,
            (&Formula::And(_, _), &Rule::LAnd) | (&Formula::And(_, _), &Rule::RAnd) => true,
            (&Formula::Or(_, _), &Rule::LOr) | (&Formula::Or(_, _), &Rule::ROr) => true,
            (&Formula::Implication(_, _), &Rule::LImpl) | (&Formula::Implication(_, _), &Rule::RImpl) => true,
            _ => false
        })
    }

    fn l_not_rule(&mut self) -> bool {
        match ProofNode::find_formula(&self.expr.l, Rule::LNot) {
            Some((i, &Formula::Not(ref g))) => {
                let mut expr = self.expr.clone();
                expr.l.remove(i);
                expr.r.push(*g.clone());

                self.rule = Some(Rule::LNot);
                self.proof_by = Some(vec![Box::from(ProofNode::new(expr))]);
                true
            }
            _ => false,
        }
    }

    fn l_and_rule(&mut self) -> bool {
        match ProofNode::find_formula(&self.expr.l, Rule::LAnd) {
            Some((i, &Formula::And(ref f, ref g))) => {
                let mut expr = self.expr.clone();
                expr.l.remove(i);
                expr.l.push(*f.clone());
                expr.l.push(*g.clone());

                self.rule = Some(Rule::LAnd);
                self.proof_by = Some(vec![Box::from(ProofNode::new(expr))]);
                true
            }
            _ => false,
        }
    }

    fn l_or_rule(&mut self) -> bool {
        match ProofNode::find_formula(&self.expr.l, Rule::LOr) {
            Some((i, &Formula::Or(ref f, ref g))) => {
                let mut expr1 = self.expr.clone();
                expr1.l.remove(i);

                let mut expr2 = expr1.clone();
                expr1.l.push(*f.clone());
                expr2.l.push(*g.clone());

                self.rule = Some(Rule::LOr);
                self.proof_by = Some(vec![Box::from(ProofNode::new(expr1)),
                                          Box::from(ProofNode::new(expr2))]);
                true
            }
            _ => false,
        }
    }

    fn l_impl_rule(&mut self) -> bool {
        match ProofNode::find_formula(&self.expr.l, Rule::LImpl) {
            Some((i, &Formula::Implication(ref f, ref g))) => {
                let mut expr1 = self.expr.clone();
                expr1.l.remove(i);

                let mut expr2 = expr1.clone();
                expr1.r.push(*f.clone());
                expr2.l.push(*g.clone());

                self.rule = Some(Rule::LImpl);
                self.proof_by = Some(vec![Box::from(ProofNode::new(expr1)),
                                          Box::from(ProofNode::new(expr2))]);
                true
            }
            _ => false,
        }
    }

    fn r_not_rule(&mut self) -> bool {
        match ProofNode::find_formula(&self.expr.r, Rule::RNot) {
            Some((i, &Formula::Not(ref f))) => {
                let mut expr = self.expr.clone();
                expr.r.remove(i);
                expr.l.push(*f.clone());

                self.rule = Some(Rule::RNot);
                self.proof_by = Some(vec![Box::from(ProofNode::new(expr))]);
                true
            }
            _ => false,
        }
    }

    fn r_and_rule(&mut self) -> bool {
        match ProofNode::find_formula(&self.expr.r, Rule::RAnd) {
            Some((i, &Formula::And(ref f, ref g))) => {
                let mut expr1 = self.expr.clone();
                expr1.r.remove(i);

                let mut expr2 = expr1.clone();
                expr1.r.push(*f.clone());
                expr2.r.push(*g.clone());

                self.rule = Some(Rule::RAnd);
                self.proof_by = Some(vec![Box::from(ProofNode::new(expr1)),
                                          Box::from(ProofNode::new(expr2))]);
                true
            }
            _ => false,
        }
    }

    fn r_or_rule(&mut self) -> bool {
        match ProofNode::find_formula(&self.expr.r, Rule::ROr) {
            Some((i, &Formula::Or(ref f, ref g))) => {
                let mut expr = self.expr.clone();
                expr.r.remove(i);
                expr.r.push(*f.clone());
                expr.r.push(*g.clone());

                self.rule = Some(Rule::ROr);
                self.proof_by = Some(vec![Box::from(ProofNode::new(expr))]);
                true
            }
            _ => false,
        }
    }

    fn r_impl_rule(&mut self) -> bool {
        match ProofNode::find_formula(&self.expr.r, Rule::RImpl) {
            Some((i, &Formula::Implication(ref f, ref g))) => {
                let mut expr = self.expr.clone();
                expr.r.remove(i);
                expr.l.push(*f.clone());
                expr.r.push(*g.clone());

                self.rule = Some(Rule::RImpl);
                self.proof_by = Some(vec![Box::from(ProofNode::new(expr))]);
                true
            }
            _ => false,
        }
    }
}