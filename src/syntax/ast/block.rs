use super::Statement;
use crate::diagnostics::Span;

pub struct Block {
    stmts: Vec<Statement>,
    span: Span,
}
