use crate::parser::ast;
use crate::lexer;
use crate::logger::logger::{Error, Logger};
use crate::parser::{ ast::{ Node }, parser::Parser };

impl Parser<'_> {
    // Returns Node::Nodes
    pub fn std_statement_parse<'a>(parser: &mut Parser<'a>) -> Result<ast::Node, Error> {
        let position = parser.lexer.get_pos();
        let mut items: Vec<Node> = Vec::new();
        let mut errors: Vec<Error> = Vec::new();
        
        parser.next(lexer::TokenType::LCP, position, false)?;

        loop {
            if parser.lexer.peek()?.token == lexer::TokenType::RCP {
                break
            }
            let fail;
            match parser.parse_non_terminal() {
                Ok(non_term) => { fail = false; items.push(ast::Node::NonTerminal(non_term)) },
                Err(e) => { 
                    errors.push(e);
                    match parser.parse_terminal() {
                        Ok(term) => { fail = false; items.push(ast::Node::Terminal(term)) },
                        Err(e) => { fail = true; errors.push(e) }
                    }
                }
            }

            if fail {
                return Err( Logger::longest(errors) );
            }

            if parser.lexer.peek()?.token == lexer::TokenType::COMMA {
                parser.lexer.advance()?;
            } else {
                break
            }
        }

        parser.next(lexer::TokenType::RCP, position, false)?;

        Ok(ast::Node::Nodes( ast::Nodes {
            nodes: items,
            pos: parser.position(position)
        }))
    }

    // Returns Node::Nodes
    pub fn std_statement_run<'a>(parser: &mut Parser<'a>) -> Result<ast::Node, Error> {
        let position = parser.lexer.get_pos();

        let mut items: Vec<Node> = Vec::new();
        for node in parser.block()?.nodes {
            items.push(node.into_node());
        }

        Ok(ast::Node::Nodes( ast::Nodes {
            nodes: items,
            pos: parser.position(position)
        }))
    }

    // Returns CustomStatement object
    pub fn std_statement_statement<'a>(parser: &mut Parser<'a>, ) /* -> Result<ast::Node, Error> */ {

    }
}
