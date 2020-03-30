use crate::parser::ast;
use crate::lexer;
use crate::logger::logger::{Error, ErrorType, Logger, ErrorDisplayType, ErrorAnnotation};
use crate::parser::ast::Node;
use crate::helpers;

/// Recursive descent parser
pub struct Parser {
    /// Lexer object
    pub lexer: lexer::Lexer,
    // Abstract syntax tree
    pub ast: Option<ast::Block>,
}

impl Parser {
    /// Return a new parser object.
    /// 
    /// Arguments
    /// 
    /// * `l`: lexer to use
    /// * `log`: logger to use
    pub fn new(l: lexer::Lexer) -> Parser {
        Parser { lexer: l, ast: None }
    }

    /// Parse from lexer
    /// 
    /// Returns nothing
    pub fn parse(&mut self) -> Result<(), Vec<Error>> {
        let position = self.lexer.get_pos();
        let statements: [fn (&mut Self) -> Result<Box<dyn Node>, Error>; 1] = [ Parser::function_define ];

        let mut ast_list: Vec<Box<dyn Node>> = Vec::new();
        if self.lexer.peek().token != lexer::TokenType::EOF {
            loop {
                let mut errors: Vec<Error> = Vec::new();
                let mut fail = true;
                for i in 0..statements.len() {
                    let statement_ast = statements[i](self);
                    match statement_ast {
                        Ok(ast_production) => { 
                            fail = false; 
                            errors.clear();
                            ast_list.push(ast_production);
                            break
                        },
                        Err(e) => errors.push(e)
                    }
                }
                
                if self.lexer.peek().token == lexer::TokenType::EOF && !fail {
                    // We've successfully parsed, break
                    break
                } else if errors.len() != 0 && fail {
                    // We've found an error, return the error
                    return Err(errors);
                }
            }
        }

        let block = ast::Block { nodes: ast_list, pos: helpers::Pos { s: position.0, e: self.lexer.position } };
        println!("{:?}", block);

        self.ast = Some(block);
        Ok(())
    }

    /// Template for syntax error
    fn syntax_error(&self, t: lexer::Token, message: &str) -> Error {
        Error::new(
            String::from(
                format!("{}, found {}", message, t)
            ),
            ErrorType::Syntax, 
            t.pos,
            ErrorDisplayType::Error,
            self.lexer.filename.clone(),
            vec![
                ErrorAnnotation::new(Some("Some Info".to_string()), helpers::Pos { s: 26, e: 35 }, ErrorDisplayType::Info, self.lexer.filename.clone()),
                ErrorAnnotation::new(Some("unexpected token".to_string()), t.pos, ErrorDisplayType::Error, self.lexer.filename.clone()),
                ErrorAnnotation::new(Some("Some warning".to_string()), helpers::Pos { s: 10, e: 12 }, ErrorDisplayType::Warning, self.lexer.filename.clone()),
                ErrorAnnotation::new(Some("An error".to_string()), helpers::Pos { s: 20, e: 25 }, ErrorDisplayType::Error, self.lexer.filename.clone()),
                ErrorAnnotation::new(Some("An error".to_string()), helpers::Pos { s: 70, e: 72 }, ErrorDisplayType::Error, self.lexer.filename.clone()),
                ErrorAnnotation::new(Some("An error".to_string()), helpers::Pos { s: 90, e: 92 }, ErrorDisplayType::Error, self.lexer.filename.clone()),
            ]
        )
    }

    /// Validate next token
    fn next(&mut self, token_type: lexer::TokenType, error_message: &str, position: (usize, usize)) -> Result<(), Error> {
        let t = self.lexer.advance().clone();
        
        if &t.token != &token_type {
            self.lexer.set_pos(position);
            Err(self.syntax_error(t, error_message))
        } else {
            Ok(())
        }
    }

    /// Parse basic block
    fn block(&mut self) -> Result<ast::Block, Error> {
        let position = self.lexer.get_pos();

        let statements: [fn (&mut Self) -> Result<Box<dyn Node>, Error>; 3] = [ Parser::function_define, Parser::variable_declaration, Parser::expression_statement ];
        
        self.next(lexer::TokenType::LCP, "expected `{`", position)?;
        let mut ast_list: Vec<Box<dyn Node>> = Vec::new();
        loop {
            let mut errors: Vec<Error> = Vec::new();
            let mut fail = true;
            for i in 0..statements.len() {
                let statement_ast = statements[i](self);
                match statement_ast {
                    Ok(ast_production) => { 
                        fail = false; 
                        errors.clear();
                        ast_list.push(ast_production);
                        break
                    },
                    Err(e) => errors.push(e)
                }
            }

            if self.lexer.peek().token == lexer::TokenType::RCP && !fail {
                // We've successfully parsed, break
                break
            } else if errors.len() != 0 && fail {
                // We've found an error, raise the error
                return Err(Logger::longest(errors));
            } else if self.lexer.peek().token != lexer::TokenType::RCP && fail {
                // We've forgotten closing brace
                break
            }
        }

        // Check for closing brace here
        self.next(lexer::TokenType::RCP, "expected `}`", position)?;

        Ok( ast::Block {
            nodes: ast_list,
            pos: helpers::Pos {
                s: position.0,
                e: self.lexer.position
            }
        } )
    }

    /// Parse function definition
    fn function_define(&mut self) -> Result<Box<dyn Node>, Error> {
        let position = self.lexer.get_pos();
        
        self.next(lexer::TokenType::DEF, "expected `def` keyword", position)?;

        let id = self.name_id()?;

        self.next(lexer::TokenType::LP, "expected `(`", position)?;

        let arguments = self.parse_arguments()?;

        self.next(lexer::TokenType::RP, "expected `)`", position)?;

        let mut return_type = ast::Type {
            value: ast::TypeType::Tuple(Vec::new()),
            pos: helpers::Pos {
                s: self.lexer.position,
                e: self.lexer.position,
            }
        };

        if self.lexer.peek().token == lexer::TokenType::ARROW {
            self.lexer.advance();
            return_type = self.type_expr()?;
        }

        let block = self.block()?;
        
        return Ok(Box::new(ast::FunctionDefine {
            return_type,
            arguments: arguments,
            block,
            name: id,
            pos: helpers::Pos {
                s: position.0,
                e: self.lexer.position
            }
        }));
    }

    fn parse_arguments(&mut self) -> Result<ast::Arguments, Error> {
        let position = self.lexer.get_pos();
        let mut positional_args: Vec<(ast::NameID, ast::Type)> = Vec::new();

        loop {
            if self.lexer.peek().token == lexer::TokenType::RP {
                // No error, we've reached the end
                break
            }

            let id = self.name_id()?;

            self.next(lexer::TokenType::COLON, "expected `:`", position)?;

            let arg_type = self.type_expr()?;

            positional_args.push((id, arg_type));
            
            if self.lexer.peek().token == lexer::TokenType::COMMA {
                self.lexer.advance();
            } else {
                break
            }
        }

        Ok(ast::Arguments { 
            positional: positional_args,
            pos: helpers::Pos {
                s: position.0, 
                e: self.lexer.position
            }
        })
    }

    /// Expressions statement
    fn expression_statement(&mut self) -> Result<Box<dyn ast::Node>, Error> {
        let position = self.lexer.get_pos();

        let expr = self.expr()?;

        self.next(lexer::TokenType::SEMI, "expected `;`", position)?;

        Ok(Box::new(ast::ExpressionStatement {
            expression: expr,
            pos: helpers::Pos {
                s: position.0,
                e: self.lexer.position
            }
        }))
    }

    /// Ful variable assign with type declaration and expression
    fn variable_assign_full(&mut self) -> Result<Box<dyn ast::Expr>, Error> {
        let position = self.lexer.get_pos();
        self.next(lexer::TokenType::LET, "expected `let` keyword", position)?;

        let var_name = self.name_id()?;

        self.next(lexer::TokenType::COLON, "expected `:`", position)?;
        
        let var_type = self.type_expr()?;

        self.next(lexer::TokenType::EQUALS, "expected `=`", position)?;
        
        let expr = self.expr()?;

        Ok(Box::new(ast::VariableAssignDeclaration {
            t: var_type,
            name: var_name,
            expr: expr,
            pos: helpers::Pos {
                s: position.0,
                e: self.lexer.position
            }
        }))
    }

    /// Variable Declaration
    fn variable_declaration(&mut self) -> Result<Box<dyn ast::Node>, Error> {
        let position = self.lexer.get_pos();
        self.next(lexer::TokenType::LET, "expected `let` keyword", position)?;

        let var_name = self.name_id()?;

        self.next(lexer::TokenType::COLON, "expected `:`", position)?;
        
        let var_type = self.type_expr()?;

        self.next(lexer::TokenType::SEMI, "expected `;`", position)?;

        Ok(Box::new(ast::VariableDeclaration {
            t: var_type,
            name: var_name,
            pos: helpers::Pos {
                s: position.0,
                e: self.lexer.position
            }
        }))
    }

    /// Variable assign with only expression
    fn variable_assign(&mut self) -> Result<Box<dyn ast::Expr>, Error> {
        let position = self.lexer.get_pos();

        let var_name = self.name_id()?;

        self.next(lexer::TokenType::EQUALS, "expected `=`", position)?;
        
        let expr = self.expr()?;

        Ok(Box::new(ast::VariableAssign {
            name: var_name,
            expr: expr,
            pos: helpers::Pos {
                s: position.0,
                e: self.lexer.position
            }
        }))
    }

    /// Top level expression
    fn expr(&mut self) -> Result<Box<dyn ast::Expr>, Error> {
        let position = self.lexer.get_pos();

        let mut left = self.term()?;

        loop {
            match self.lexer.peek().token {
                lexer::TokenType::ADD  => { 
                    self.lexer.advance();
                    let right = self.term()?;

                    left = Box::new(ast::Add {
                        left,
                        right,
                        pos: helpers::Pos {
                            s: position.0,
                            e: self.lexer.position,
                        }
                    });
                },

                lexer::TokenType::SUB => {
                    self.lexer.advance();
                    let right = self.term()?;
                    
                    left = Box::new(ast::Sub {
                        left,
                        right,
                        pos: helpers::Pos {
                            s: position.0,
                            e: self.lexer.position,
                        }
                    });
                },

                _  => { return Ok(left); }
            }
        }
    }

    fn term(&mut self) -> Result<Box<dyn ast::Expr>, Error> {
        let position = self.lexer.get_pos();
        
        let mut left = self.factor()?;
        
        loop {
            match self.lexer.peek().token {
                lexer::TokenType::MUL  => { 
                    self.lexer.advance();
                    let right = self.factor()?;
                    
                    left = Box::new(ast::Mul {
                        left,
                        right,
                        pos: helpers::Pos {
                            s: position.0,
                            e: self.lexer.position,
                        }
                    });
                },

                lexer::TokenType::DIV => {
                    self.lexer.advance();
                    let right = self.factor()?;
                    
                    left = Box::new(ast::Div {
                        left,
                        right,
                        pos: helpers::Pos {
                            s: position.0,
                            e: self.lexer.position,
                        }
                    });
                },

                lexer::TokenType::MOD  => {
                    self.lexer.advance();
                    let right = self.factor()?;
                    
                    left = Box::new(ast::Mod {
                        left,
                        right,
                        pos: helpers::Pos {
                            s: position.0,
                            e: self.lexer.position,
                        }
                    });
                },

                _  => { return Ok(left); }
            }
        }
    }

    fn factor(&mut self) -> Result<Box<dyn ast::Expr>, Error> {
        let position = self.lexer.get_pos();
        
        match self.next(lexer::TokenType::SUB, "expected `-`", position) {
            Ok(_) => {
                match self.item() {
                    Ok(item) => Ok(Box::new(
                        ast::Neg { 
                            value: item, 
                            pos: helpers::Pos {
                                s: position.0,
                                e: self.lexer.position
                            } 
                        }
                    )),
                    Err(e) => {
                        self.lexer.set_pos(position);
                        Err(e)
                    }
                }
            },
            Err(_) => {
                match self.item() {
                    Ok(item) => Ok(item),
                    Err(e) => {
                        self.lexer.set_pos(position);
                        Err(e)
                    }
                }
            }
        }
    }

    fn item(&mut self) -> Result<Box<dyn ast::Expr>, Error> {
        let position = self.lexer.get_pos();

        if let Ok(ast) = self.integer() {
            return Ok(ast);
        }

        if let Ok(ast) = self.variable_assign() {
            return Ok(ast);
        }

        if let Ok(ast) = self.variable_assign_full() {
            return Ok(ast);
        }
        
        if let lexer::TokenType::LP = self.lexer.advance().token {
            let expr = self.expr()?;
            if let lexer::TokenType::RP = self.lexer.advance().token { 
                return Ok(expr);
            }
            let next_tok = self.lexer.peek().clone();
            return Err(self.syntax_error(next_tok, "expected `)`"));
        }

        self.lexer.set_pos(position);
        let next_tok = self.lexer.peek().clone();
        Err(self.syntax_error(next_tok, "expected expression"))
    }

    fn integer(&mut self) -> Result<Box<ast::Integer>, Error> {
        let position = self.lexer.get_pos();

        let int = self.lexer.advance().clone();
        if let lexer::TokenType::NUMBER(value) = &int.token {
            Ok(
                Box::new(ast::Integer {
                    value: value.to_string(),
                    pos: int.pos.clone()
                })
            )
        } else {
            self.lexer.set_pos(position);
            Err(self.syntax_error(int.clone(), "expected integer"))
        }
    }

    /// Parse name identifier (i.e. function name)
    fn name_id(&mut self) -> Result<ast::NameID, Error> {
        let position = self.lexer.get_pos();
        let id = self.lexer.advance().clone();
        if let lexer::TokenType::IDENTIFIER(value) = &id.token {
            Ok(
                ast::NameID {
                    value: value.to_string(),
                    pos: id.pos.clone()
                }
            )
        } else {
            self.lexer.set_pos(position);
            Err(self.syntax_error(id.clone(), "expected identifier"))
        }
    }

    /// Parse type expression
    fn type_expr(&mut self) -> Result<ast::Type, Error> {
        let position = self.lexer.get_pos();
        let id = self.lexer.advance().clone();
        if let lexer::TokenType::IDENTIFIER(value) = &id.token {
            return Ok(
                ast::Type {
                    value: ast::TypeType::Type(value.to_string()),
                    pos: id.pos.clone()
                }
            );
        }
        
        self.lexer.set_pos(position);
        Err(self.syntax_error(id.clone(), "expected identifier"))
    }

}
