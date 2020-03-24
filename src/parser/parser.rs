use crate::parser::ast;
use crate::lexer;
use crate::logger::{Error, ErrorType, Logger};
use crate::parser::ast::Node;
use crate::helpers;

pub struct Parser<'a> {
    pub lexer: lexer::Lexer<'a>,
    logger: Logger<'a>,
    ast: Option<ast::Block>
}

fn variant_eq<T>(a: &T, b: &T) -> bool {
    std::mem::discriminant(a) == std::mem::discriminant(b)
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

    fn syntax_error(t: lexer::Token, message: &str) -> Vec<Error> {
        vec![
            Error::new(
                String::from(
                    message
                ),
                ErrorType::Syntax, 
                t.pos.clone(),
                Some(t),
                String::from(
                    "single"
                )
            )
        ]
    }

    fn next(&mut self, token_type: lexer::TokenType, error_message: &str, position: (i64, i64)) -> Result<(), Vec<Error>> {
        let t = self.lexer.advance().clone();
        
        if &t.token != &token_type {
            self.lexer.set_pos(position);
            Err(Parser::syntax_error(t, error_message))
        } else {
            Ok(())
        }
    }

    fn block(&mut self) -> Result<ast::Block, Vec<Error>> {
        let position = self.lexer.get_pos();
        let mut statements = [
            || self.function_define()
        ];
        
        self.next(lexer::TokenType::RCP, "Expected `{`", position)?;
        let mut ast_list: Vec<Box<dyn Node>> = Vec::new();
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
                        ast_list.push(ast.unwrap());
                        break
                    },
                    Err(e) => errors.push(e)
                }
            }
            

            if self.lexer.peek().token == lexer::TokenType::RCP && !fail {
                // We've successfully parsed, break
                break
            } else if self.lexer.peek().token != lexer::TokenType::RCP && fail {
                // We've forgotten closing brace
                break
            } else if errors.len() != 0 && fail {
                // We've found an error, raise the error
                return Err(errors.remove(0)); // TODO: add logger static method that checks for longest parsing error
            } 
        }

        // Check for closing brace here
        self.next(lexer::TokenType::RCP, "Expected `}`", position)?;

        Ok(ast::Block {
            nodes: ast_list,
            pos: helpers::Pos {
                s: position.0,
                e: self.lexer.position
            }
        })
    }

    fn function_define(&mut self) -> Result<ast::FunctionDefine, Vec<Error>> {
        let position = self.lexer.get_pos();
        
        self.next(lexer::TokenType::DEF, "Expected `def` keyword", position)?;

        let id = self.name_id()?;

        self.next(lexer::TokenType::RP, "Expected `(`", position)?;

        // TODO: add function arguments

        self.next(lexer::TokenType::LP, "Expected `)`", position)?;

        let mut return_type = ast::Type {
            value: ast::TypeType::Tuple(Vec::new()),
            pos: helpers::Pos {
                s: self.lexer.position,
                e: self.lexer.position,
            }
        };

        if self.lexer.peek().token == lexer::TokenType::ARROW {
            return_type = self.type_expr()?;
        }

        let block = self.block()?;
        
        return Ok(ast::FunctionDefine {
            return_type,
            arguments: ast::Arguments{
                positional: Vec::new(),
                pos: helpers::Pos {
                    s: 0,
                    e: 0
                }
            },
            block,
            name: id,
            pos: helpers::Pos {
                s: position.0,
                e: self.lexer.position
            }
        });
    }

    fn name_id(&mut self) -> Result<ast::NameID, Vec<Error>> {
        let position = self.lexer.get_pos();
        let id = self.lexer.advance();
        if let lexer::TokenType::IDENTIFIER(value) = &id.token {
            Ok(
                ast::NameID {
                    value: value.to_string(),
                    pos: id.pos.clone()
                }
            )
        } else {
            self.lexer.set_pos(position);
            Err(Parser::syntax_error(id.clone(), "Expected identifier"))
        }
    }

    fn type_expr(&mut self) -> Result<ast::Type, Vec<Error>> {
        let position = self.lexer.get_pos();
        let id = self.lexer.advance();
        if let lexer::TokenType::IDENTIFIER(value) = &id.token {
            return Ok(
                ast::Type {
                    value: ast::TypeType::Type(value.to_string()),
                    pos: id.pos.clone()
                }
            );
        }
        
        self.lexer.set_pos(position);
        Err(Parser::syntax_error(id.clone(), "Expected identifier"))
    }

}
