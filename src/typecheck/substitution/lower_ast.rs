//! Lower TypedAst to mir (middle intermediate representation)

use super::mir;
use crate::helpers;
use crate::typecheck::annotation::{TypedExpr, TypedStmt};
use crate::typecheck::unifier::Substitutions;

pub struct TypedAstLower {}

impl TypedAstLower {
    pub fn new() -> Self {
        Self {}
    }

    pub fn lower(self, ast: Vec<TypedStmt>, substitutions: Substitutions) -> Vec<mir::MirStmt> {
        Vec::new()
    }
}
