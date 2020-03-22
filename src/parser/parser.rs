use crate::parser::ast;
use crate::lexer;

pub struct Parser<'a> {
    pub lexer: lexer::Lexer<'a>,
    ast: Option<ast::Block>
}

impl Parser<'_> {
    pub fn new<'a> (l: lexer::Lexer<'a>) -> Parser<'a> {
        Parser { lexer: l, ast: None }
    }

    pub fn parse() {
        loop {
            
        }
    }

    fn function(&self) {
        
    }


}
