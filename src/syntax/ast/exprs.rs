use super::Name;
use crate::common::Str;
use crate::diagnostics::Span;

#[derive(Debug)]
#[cfg_attr(test, derive(Derivative))]
#[cfg_attr(test, derivative(PartialEq))]
pub struct Expr {
    pub kind: ExprKind,
    #[cfg_attr(test, derivative(PartialEq = "ignore"))]
    pub span: Span,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum ExprKind {
    Integer(Str),
    String(Str),
    Identifier(Name),
}
