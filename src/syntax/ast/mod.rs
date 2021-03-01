mod block;
mod exprs;
mod statements;
mod types;

use crate::common::Str;
use crate::diagnostics::Span;
use smallvec::SmallVec;

pub use block::*;
pub use exprs::*;
pub use statements::*;
pub use types::*;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct RootAst {
    pub stmts: Vec<Statement>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(Derivative))]
#[cfg_attr(test, derivative(PartialEq))]
pub struct Name {
    pub items: SmallVec<[Str; 1]>,
    #[cfg_attr(test, derivative(PartialEq = "ignore"))]
    pub span: Span,
}

#[derive(Debug)]
#[cfg_attr(test, derive(Derivative))]
#[cfg_attr(test, derivative(PartialEq))]
pub struct Ident {
    pub s: Str,
    #[cfg_attr(test, derivative(PartialEq = "ignore"))]
    pub span: Span,
}

impl Ident {
    pub fn new(s: Str, span: Span) -> Self {
        Self { s, span }
    }

    #[cfg(test)]
    pub fn n(s: &'static str) -> Self {
        Self {
            span: Span::dummy(),
            s: Str::from(s),
        }
    }
}
