use super::Constraints;
use crate::typecheck::types;

use std::collections::HashMap;

pub type Substitution = HashMap<usize, types::MirType>;

pub struct ConstraintSolver {}

impl ConstraintSolver {
    pub fn new() -> Self {
        Self {}
    }

    pub fn solve(&self, constraints: Constraints) -> Substitution {
        Substitution::new()
    }
}

