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

pub struct RootAst {
    stmts: Vec<Statement>,
}

pub struct Name {
    items: SmallVec<[Str; 1]>,
    span: Span,
}

pub struct Ident {
    s: Str,
}
