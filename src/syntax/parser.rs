use std::iter::Peekable;

use super::ast::*;
use super::Lexer;
use crate::diagnostics::DiagnosticsPrelude::*;

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            lexer: lexer.peekable(),
        }
    }

    pub fn parse(&mut self) -> Failible<RootAst> {
        todo!()
    }
}
