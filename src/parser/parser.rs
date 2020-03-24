use crate::parser::ast;
use crate::lexer;
use crate::logger::{Error, ErrorType, Logger};
use crate::parser::ast::Node;

pub struct Parser<'a> {
    pub lexer: lexer::Lexer<'a>,
    logger: Logger<'a>,
    ast: Option<ast::Block>
}

impl Parser<'_> {
    pub fn new<'a> (l: lexer::Lexer<'a>, log: Logger<'a>) -> Parser<'a> {
        Parser { lexer: l, ast: None, logger: log }
    }

    pub fn parse(&mut self) {
        let mut statements = [
            || self.function_define(),
        ];

        loop {
            let mut errors: Vec<Vec<Error>> = Vec::new();
            let mut fail = true;
            let mut ast: Option<Box<dyn Node>> = None;
            for i in 0..statements.len() {
                let statement_ast = statements[i]();
                match statement_ast {
                    Ok(ast_production) => { 
                        ast = Some(Box::new(ast_production)); 
                        fail = false; 
                        errors.clear();
                        break
                    },
                    Err(e) => errors.push(e)
                }
            }
            
            if self.lexer.peek().token == lexer::TokenType::EOF && !fail {
                // We've successfully parsed, break
                break
            } else if errors.len() != 0 && fail {
                // We've found an error, raise the error
                println!("{:?}", errors[0]);
            }
            break
        }
    }

    fn function_define(&mut self) -> Result<(), Vec<Error> > {
        let position = self.lexer.get_pos();
        
        let t = self.lexer.peek().clone();
        if t.token != lexer::TokenType::DEF {
            self.lexer.set_pos(position);
            return Err(
                vec![
                    Error::new(
                        String::from(
                            "Expected `def` keyword"
                        ),
                        ErrorType::Syntax, 
                        t.pos.clone(),
                        Some(t)
                    )
                ]
            );
        }
        
        return Ok(());
    }

}
