use super::Name;
use crate::common::Str;
use crate::diagnostics::Span;

pub struct Expr {
    kind: ExprKind,
    span: Span,
}

pub enum ExprKind {
    Integer(Str),
    String(Str),
    Identifier(Name),
}
