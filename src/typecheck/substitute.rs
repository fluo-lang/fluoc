use super::annotation::*;
use super::unifier::Substitutions;
use crate::logger::ErrorValue;

impl TypedStmt {
    fn substitute(self, solved_constraints: &Substitutions) -> Result<Self, ErrorValue> {
        Ok(self)
    }
}

pub fn substitute(stmts: Vec<TypedStmt>, solved_constraints: Substitutions) -> Result<Vec<TypedStmt>, Vec<ErrorValue>> {
    let mut final_stmts = Vec::with_capacity(stmts.len());
    let mut errors = Vec::new();

    for stmt in stmts.into_iter() {
        match stmt.substitute(&solved_constraints) {
            Ok(val) => final_stmts.push(val),
            Err(err) => errors.push(err)
        }
    }

    if errors.is_empty() {
        Ok(final_stmts)
    } else {
        Err(errors)
    }
}
