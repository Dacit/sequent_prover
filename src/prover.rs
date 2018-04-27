use std;
use std::cell::RefCell;
use super::dt::{Expression, Formula, ProofNode, Rule};

fn ax_rule(n: &ProofNode) -> Option<Box<Fn(ProofNode) -> (Rule, Vec<ProofNode>)>> {
    let left_atoms = n.expr.l.iter().filter().collect();
    let right_atoms = n.expr.r.iter().filter();
    if right_atoms.intersects(left_atoms) {
        Some(Box::new(|_| (Rule::AX, vec![])))
    } else {
        None
    }
}

pub fn proof(expr: &Expression) -> ProofNode {
    let mut proof_tree = ProofNode::new(&expr);

    {
        let mut unproved_nodes = vec![&proof_tree];
        //unproved_nodes.find(ax_rule)
    }

    proof_tree
}