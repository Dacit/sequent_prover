use std::collections::HashMap;
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
pub struct ProofNode<'a> {
    pub expr: &'a Expression,
    pub rule: Option<Rule>,
    pub proof_by: Option<Vec<ProofNode<'a>>>,
}

impl<'a> ProofNode<'a> {
    pub fn new(e: &Expression) -> ProofNode {
        ProofNode { expr: e, rule: None, proof_by: None }
    }

    pub fn ax_rule(&mut self) -> Option<Vec<&mut ProofNode>> {
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
            Some(vec![])
        } else {
            None
        }
    }

    pub fn l_not_rule(&mut self) -> Option<Vec<&mut ProofNode>> {
        unimplemented!()
    }
    pub fn l_and_rule(&mut self) -> Option<Vec<&mut ProofNode>> {
        unimplemented!()
    }
    pub fn l_or_rule(&mut self) -> Option<Vec<&mut ProofNode>> {
        unimplemented!()
    }
    pub fn l_impl_rule(&mut self) -> Option<Vec<&mut ProofNode>> {
        unimplemented!()
    }
    pub fn r_not_rule(&mut self) -> Option<Vec<&mut ProofNode>> { unimplemented!() }
    pub fn r_and_rule(&mut self) -> Option<Vec<&mut ProofNode>> {
        unimplemented!()
    }
    pub fn r_or_rule(&mut self) -> Option<Vec<&mut ProofNode>> {
        unimplemented!()
    }
    pub fn r_impl_rule(&mut self) -> Option<Vec<&mut ProofNode>> {
        unimplemented!()
    }
}

/*pub fn proof2(expr: &Expression) -> ProofNode {
    let mut proof_tree = ProofNode::new(&expr);
    let using_rules = [ProofNode::ax_rule];
    {
        let mut unproved_nodes = &vec![&mut proof_tree];

        while !unproved_nodes.is_empty() {
            let nodes = *unproved_nodes.into_iter();
            unproved_nodes = &vec![];

            'nodes: for node in nodes {
                for rule in using_rules.iter() {
                    let res = rule(node);
                    if res.is_some() {
                        *unproved_nodes.append(&mut res.unwrap());
                        continue 'nodes;
                    }
                }
                panic!("Could not prove");
            }
        }
    }
    proof_tree
}*/

pub fn proof(expr: &Expression) -> (bool, ProofNode) {
    let mut root = ProofNode::new(&expr);

    let is_valid = proof_tree(&mut root);

    (is_valid, root)
}

fn proof_tree(node: &mut ProofNode) -> bool {
    let RULES = [ProofNode::ax_rule, ProofNode::l_not_rule, ProofNode::l_and_rule, ProofNode::r_not_rule, ProofNode::r_or_rule, ProofNode::r_impl_rule, ProofNode::l_or_rule, ProofNode::l_impl_rule, ProofNode::r_and_rule];

    for rule in RULES.iter() {
        let res = rule(node);
        if res.is_some() {
            return res.unwrap().into_iter().map(|n| proof_tree(n)).fold(true, |f, g| f && g);
        }
    }
    false
}