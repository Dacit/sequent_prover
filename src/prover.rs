use super::parser::{Expression, Formula};
use std::clone::Clone;

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum Rule {
    Ax,
    LFalse,
    LNot,
    LAnd,
    LOr,
    LImpl,
    RNot,
    RAnd,
    ROr,
    RImpl,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Proof {
    pub rule: Rule,
    pub leaves: Vec<ProofNode>,
}

impl Proof {
    pub fn new(r: Rule, l: Vec<ProofNode>) -> Proof {
        Proof { rule: r, leaves: l }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct ProofNode {
    pub expr: Expression,
    pub proof_by: Option<Proof>,
}

static RULES: [fn(&mut ProofNode) -> Option<Proof>; 10] = [
    ProofNode::ax_rule,
    ProofNode::l_false_rule,
    ProofNode::l_not_rule,
    ProofNode::l_and_rule,
    ProofNode::r_not_rule,
    ProofNode::r_or_rule,
    ProofNode::r_impl_rule,
    ProofNode::l_or_rule,
    ProofNode::l_impl_rule,
    ProofNode::r_and_rule,
];

impl ProofNode {
    pub fn new(e: Expression) -> ProofNode {
        ProofNode {
            expr: e,
            proof_by: None,
        }
    }

    /// Builds proof tree starting at self, returns if all leaves could be proven
    pub fn prove(&mut self) -> bool {
        self.proof_by = RULES.iter().filter_map(|rule| rule(self)).nth(0);
        self.proof_by
            .as_mut()
            .and_then(|proof| {
                Some(
                    proof.leaves.iter_mut()
                        .map(ProofNode::prove)
                        // Do NOT use .all() since that does not evaluate all expressions
                        .fold(true, |f, g| f && g),
                )
            })
            .unwrap_or(false)
    }

    fn destruct_formula<F>(fs: &[Formula], rule: F) -> Option<Proof>
        where F: Fn(usize, &Formula) -> Option<Proof> {
        fs.iter()
            .enumerate()
            .filter_map(|(i, fs)| rule(i, fs))
            .nth(0)
    }

    fn ax_rule(&mut self) -> Option<Proof> {
        let left_atoms: Vec<String> = self.expr
            .l
            .iter()
            .filter_map(|f| match f {
                &Formula::Atom(ref a) => Some(a.clone()),
                _ => None,
            })
            .collect();

        ProofNode::destruct_formula(&self.expr.r, |_, f| match f {
            &Formula::Atom(ref a) if left_atoms.contains(a) => Some(Proof::new(Rule::Ax, vec![])),
            _ => None,
        })
    }

    fn l_false_rule(&mut self) -> Option<Proof> {
        ProofNode::destruct_formula(&self.expr.l, |_, f| match f {
            &Formula::False => Some(Proof::new(Rule::LFalse, vec![])),
            _ => None,
        })
    }

    fn l_not_rule(&mut self) -> Option<Proof> {
        ProofNode::destruct_formula(&self.expr.l, |i, f| match f {
            &Formula::Not(ref f) => {
                let mut expr = self.expr.clone();
                expr.l.remove(i);
                expr.r.push(f.as_ref().clone());

                Some(Proof::new(Rule::LNot, vec![ProofNode::new(expr)]))
            }
            _ => None,
        })
    }

    fn l_and_rule(&mut self) -> Option<Proof> {
        ProofNode::destruct_formula(&self.expr.l, |i, f| match f {
            &Formula::And(ref f, ref g) => {
                let mut expr = self.expr.clone();
                expr.l.remove(i);
                expr.l.push(f.as_ref().clone());
                expr.l.push(g.as_ref().clone());

                Some(Proof::new(Rule::LAnd, vec![ProofNode::new(expr)]))
            }
            _ => None
        })
    }

    fn l_or_rule(&mut self) -> Option<Proof> {
        ProofNode::destruct_formula(&self.expr.l, |i, f| match f {
            &Formula::Or(ref f, ref g) => {
                let mut expr0 = self.expr.clone();
                expr0.l.remove(i);

                let mut expr1 = expr0.clone();
                expr0.l.push(f.as_ref().clone());
                expr1.l.push(g.as_ref().clone());

                Some(Proof::new(
                    Rule::LOr,
                    vec![ProofNode::new(expr0), ProofNode::new(expr1)],
                ))
            }
            _ => None
        })
    }

    fn l_impl_rule(&mut self) -> Option<Proof> {
        ProofNode::destruct_formula(&self.expr.l, |i, f| match f {
            &Formula::Implication(ref f, ref g) => {
                let mut expr1 = self.expr.clone();
                expr1.l.remove(i);

                let mut expr2 = expr1.clone();
                expr1.r.push(f.as_ref().clone());
                expr2.l.push(g.as_ref().clone());

                Some(Proof::new(
                    Rule::LImpl,
                    vec![ProofNode::new(expr1), ProofNode::new(expr2)],
                ))
            }
            _ => None,
        })
    }

    fn r_not_rule(&mut self) -> Option<Proof> {
        ProofNode::destruct_formula(&self.expr.r, |i, f| match f {
            &Formula::Not(ref f) => {
                let mut expr = self.expr.clone();
                expr.r.remove(i);
                expr.l.push(f.as_ref().clone());

                Some(Proof::new(Rule::RNot, vec![ProofNode::new(expr)]))
            }
            _ => None,
        })
    }

    fn r_and_rule(&mut self) -> Option<Proof> {
        ProofNode::destruct_formula(&self.expr.r, |i, f| match f {
            &Formula::And(ref f, ref g) => {
                let mut expr1 = self.expr.clone();
                expr1.r.remove(i);

                let mut expr2 = expr1.clone();
                expr1.r.push(f.as_ref().clone());
                expr2.r.push(g.as_ref().clone());

                Some(Proof::new(
                    Rule::RAnd,
                    vec![ProofNode::new(expr1), ProofNode::new(expr2)],
                ))
            },
            _ => None
        })
    }

    fn r_or_rule(&mut self) -> Option<Proof> {
        ProofNode::destruct_formula(&self.expr.r, |i, f| match f {
            &Formula::Or(ref f, ref g) => {
                let mut expr = self.expr.clone();
                expr.r.remove(i);
                expr.r.push(f.as_ref().clone());
                expr.r.push(g.as_ref().clone());

                Some(Proof::new(Rule::ROr, vec![ProofNode::new(expr)]))
            },
            _ => None
        })
    }

    fn r_impl_rule(&mut self) -> Option<Proof> {
        ProofNode::destruct_formula(&self.expr.r, |i, f| match f {
            &Formula::Implication(ref f, ref g) => {
                let mut expr = self.expr.clone();
                expr.r.remove(i);
                expr.l.push(f.as_ref().clone());
                expr.r.push(g.as_ref().clone());

                Some(Proof::new(Rule::RImpl, vec![ProofNode::new(expr)]))
            },
            _ => None
        })
    }
}

#[cfg(test)]
mod tests {
    use parser::*;
    use prover::*;

    #[test]
    fn test_empty_expr() {
        let (proven, _) = parse_expression("=>").prove();
        assert!(!proven);
    }

    #[test]
    fn test_complex_unprovable() {
        let (proven, _) = parse_expression(
            " => P & Q | R & Q | -P & -R | R & -R > P | Q",
        ).prove();
        assert!(!proven);
    }

    #[test]
    fn test_complex() {
        let (proven, proof_tree) = parse_expression(
            " => P & Q | R & Q | P & -R | R & -R > P | Q",
        ).prove();
        assert!(proven);
        //Assert that proof tree is complex - 5 layers min.
        assert!(
            proof_tree
                .proof_by
                .unwrap()
                .leaves
                .remove(0)
                .proof_by
                .unwrap()
                .leaves
                .remove(0)
                .proof_by
                .unwrap()
                .leaves
                .remove(0)
                .proof_by
                .unwrap()
                .leaves
                .remove(0)
                .proof_by
                .is_some()
        );
    }

    #[test]
    fn test_axiom() {
        let expr = parse_expression("A, T => A, D");
        let (proven, proof_tree) = expr.clone().prove();

        assert!(proven);
        assert_eq!(
            ProofNode {
                expr: expr,
                proof_by: Some(Proof {
                    rule: Rule::Ax,
                    leaves: vec![],
                }),
            },
            proof_tree
        );
    }

    #[test]
    fn test_l_false() {
        let expr = parse_expression("0, T => D");
        let (proven, proof_tree) = expr.clone().prove();

        assert!(proven);
        assert_eq!(
            ProofNode {
                expr: expr,
                proof_by: Some(Proof {
                    rule: Rule::LFalse,
                    leaves: vec![],
                }),
            },
            proof_tree
        );
    }

    #[test]
    fn test_l_not() {
        let expr = parse_expression("-F, F, T => D");
        let (proven, proof_tree) = expr.clone().prove();

        assert!(proven);
        assert_eq!(
            ProofNode {
                expr: expr,
                proof_by: Some(Proof {
                    rule: Rule::LNot,
                    leaves: vec![ProofNode {
                        expr: parse_expression("F, T => D, F"),
                        proof_by: Some(Proof {
                            rule: Rule::Ax,
                            leaves: vec![],
                        }),
                    }],
                }),
            },
            proof_tree
        );
    }

    #[test]
    fn test_l_or() {
        let expr = parse_expression("F | G, T => F, G, D");
        let (proven, proof_tree) = expr.clone().prove();

        assert!(proven);
        assert_eq!(
            ProofNode {
                expr: expr,
                proof_by: Some(Proof {
                    rule: Rule::LOr,
                    leaves: vec![
                        ProofNode {
                            expr: parse_expression("T, F => F, G, D"),
                            proof_by: Some(Proof {
                                rule: Rule::Ax,
                                leaves: vec![],
                            }),
                        },
                        ProofNode {
                            expr: parse_expression("T, G => F, G, D"),
                            proof_by: Some(Proof {
                                rule: Rule::Ax,
                                leaves: vec![],
                            }),
                        },
                    ],
                }),
            },
            proof_tree
        );
    }

    #[test]
    fn test_l_and() {
        let expr = parse_expression("F & G, T => F");
        let (proven, proof_tree) = expr.clone().prove();

        assert!(proven);
        assert_eq!(
            ProofNode {
                expr: expr,
                proof_by: Some(Proof {
                    rule: Rule::LAnd,
                    leaves: vec![ProofNode {
                        expr: parse_expression("T, F, G => F"),
                        proof_by: Some(Proof {
                            rule: Rule::Ax,
                            leaves: vec![],
                        }),
                    }],
                }),
            },
            proof_tree
        );
    }

    #[test]
    fn test_l_impl() {
        let expr = parse_expression("F > G, F, T => G, D");
        let (proven, proof_tree) = expr.clone().prove();

        assert!(proven);
        assert_eq!(
            ProofNode {
                expr: expr,
                proof_by: Some(Proof {
                    rule: Rule::LImpl,
                    leaves: vec![
                        ProofNode {
                            expr: parse_expression("F, T => G, D, F"),
                            proof_by: Some(Proof {
                                rule: Rule::Ax,
                                leaves: vec![],
                            }),
                        },
                        ProofNode {
                            expr: parse_expression("F, T, G => G, D"),
                            proof_by: Some(Proof {
                                rule: Rule::Ax,
                                leaves: vec![],
                            }),
                        },
                    ],
                }),
            },
            proof_tree
        );
    }

    #[test]
    fn test_r_not() {
        let expr = parse_expression("T => -F, F, D");
        let (proven, proof_tree) = expr.clone().prove();

        assert!(proven);
        assert_eq!(
            ProofNode {
                expr: expr,
                proof_by: Some(Proof {
                    rule: Rule::RNot,
                    leaves: vec![ProofNode {
                        expr: parse_expression("T, F => F, D"),
                        proof_by: Some(Proof {
                            rule: Rule::Ax,
                            leaves: vec![],
                        }),
                    }],
                }),
            },
            proof_tree
        );
    }

    #[test]
    fn test_r_or() {
        let expr = parse_expression("F, T => F | G, D");
        let (proven, proof_tree) = expr.clone().prove();

        assert!(proven);
        assert_eq!(
            ProofNode {
                expr: expr,
                proof_by: Some(Proof {
                    rule: Rule::ROr,
                    leaves: vec![ProofNode {
                        expr: parse_expression("F, T => D, F, G"),
                        proof_by: Some(Proof {
                            rule: Rule::Ax,
                            leaves: vec![],
                        }),
                    }],
                }),
            },
            proof_tree
        );
    }

    #[test]
    fn test_r_and() {
        let expr = parse_expression("F, G, T => F & G, D");
        let (proven, proof_tree) = expr.clone().prove();

        assert!(proven);
        assert_eq!(
            ProofNode {
                expr: expr,
                proof_by: Some(Proof {
                    rule: Rule::RAnd,
                    leaves: vec![
                        ProofNode {
                            expr: parse_expression("F, G, T => D, F"),
                            proof_by: Some(Proof {
                                rule: Rule::Ax,
                                leaves: vec![],
                            }),
                        },
                        ProofNode {
                            expr: parse_expression("F, G, T => D, G"),
                            proof_by: Some(Proof {
                                rule: Rule::Ax,
                                leaves: vec![],
                            }),
                        },
                    ],
                }),
            },
            proof_tree
        );
    }

    #[test]
    fn test_r_impl() {
        let expr = parse_expression("G, T => F > G, F, D");
        let (proven, proof_tree) = expr.clone().prove();

        assert!(proven);
        assert_eq!(
            ProofNode {
                expr: expr,
                proof_by: Some(Proof {
                    rule: Rule::RImpl,
                    leaves: vec![ProofNode {
                        expr: parse_expression("G, T, F => F, D, G"),
                        proof_by: Some(Proof {
                            rule: Rule::Ax,
                            leaves: vec![],
                        }),
                    }],
                }),
            },
            proof_tree
        );
    }
}
