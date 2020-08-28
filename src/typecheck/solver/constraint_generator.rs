use crate::typecheck::annotation::{AnnotationType, TypedExpr, TypedStmt};

use std::collections::HashSet;

pub struct Constraint {
    a: AnnotationType,
    b: AnnotationType,
}

pub type Constraints = HashSet<Constraint>;

pub struct ConstraintGenerator {}

impl ConstraintGenerator {
    pub fn new() -> Self {
        ConstraintGenerator {}
    }

    pub fn generate(&self, ast: &[TypedStmt]) -> Constraints {
        Constraints::new()
    }
}
