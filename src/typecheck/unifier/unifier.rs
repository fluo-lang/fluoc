use crate::typecheck::{constraint_gen::Constraints, types};

use std::collections::HashMap;

pub type Substitutions = HashMap<usize, types::MirType>;

pub struct Unifier {}

impl Unifier {
    pub fn new() -> Self {
        Self {}
    }

    pub fn solve(&self, constraints: Constraints<'_>) -> Substitutions {
        Substitutions::new()
    }
}
