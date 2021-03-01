use super::Statement;
use crate::diagnostics::Span;

#[derive(Debug)]
#[cfg_attr(test, derive(Derivative))]
#[cfg_attr(test, derivative(PartialEq))]
pub struct Block {
    pub stmts: Vec<Statement>,
    #[cfg_attr(test, derivative(PartialEq = "ignore"))]
    pub span: Span,
}
