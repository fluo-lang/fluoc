//! Lower TypedAst to mir (middle intermediate representation)

use super::mir;
use crate::typecheck::annotation::{TypedStmt, TypedExpr};
use crate::typecheck::solver::Substitution;
use crate::helpers;

pub struct TypedAstLower {}

impl TypedAstLower {
    pub fn new() -> Self {
        Self {}
    }

    pub fn lower(self, ast: Vec<TypedStmt>, substitutions: Substitution) -> Vec<mir::MirStmt> {
        Vec::new()
    }
}

