use crate::common::diagnostic::Spanned;

#[derive(Debug)]
pub enum Statement {
    Expression(Spanned<Expr>),
}

#[derive(Debug)]
pub enum Expr {}
