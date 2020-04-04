use crate::parser::ast;
use crate::lexer;
use crate::logger::logger::{Error, ErrorType, Logger, ErrorDisplayType, ErrorAnnotation};
use crate::parser::ast::{ Statement, Expr, Scope };
use crate::helpers;
use crate::codegen::module_codegen::CodeGenModule;

/// Recursive descent parser
pub struct Parser<'a> {
    /// Lexer object
    pub lexer: lexer::Lexer,
    /// Abstract syntax tree
    pub ast: Option<ast::Block>,
    pub modules: Vec<CodeGenModule<'a>>,
    statements: Vec<fn (&mut Self) -> Result<Statement, Error>>
}

impl Parser<'_> {
    /// Return a new parser object.
    /// 
    /// Arguments
    /// 
    /// * `l`: lexer to use
    /// * `log`: logger to use
    pub fn new<'a>(l: lexer::Lexer) -> Parser<'a> {
        Parser { lexer: l, ast: None, modules: Vec::new(), statements: vec![Parser::function_define, Parser::expression_statement, Parser::variable_declaration]}
    }

    /// Parse from lexer
    /// 
    /// Returns nothing
    pub fn parse(&mut self) -> Result<(), Vec<Error>> {
        let position = self.lexer.get_pos();

        // Set our scope to outside
        let scope = Scope::Outer;

        let mut ast_list: Vec<Statement> = Vec::new();
        if self.lexer.peek().token != lexer::TokenType::EOF {
            loop {
                let mut errors: Vec<Error> = Vec::new();
                let mut fail = true;
                for i in 0..self.statements.len() {
                    let statement_ast = self.statements[i](self);
                    match statement_ast {
                        Ok(ast_production) => {
                            if ast_production.in_scope(&scope) {
                                fail = false; 
                                errors.clear();
                                ast_list.push(ast_production);
                                break
                            } else {
                                break
                            }
                        },
                        Err(e) => errors.push(e)
                    }
                }
                
                if self.lexer.peek().token == lexer::TokenType::EOF && !fail {
                    // We've successfully parsed, break
                    break
                } else if !errors.is_empty() && fail {
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
    fn syntax_error(&self, t: lexer::Token, message: &str, is_keyword: bool) -> Error {
        Error::new(
            String::from(
                if !is_keyword { 
                    format!("{}, found {}", message, t) 
                } else {  
                    format!("unexpected {}", t) 
                }
            ),
            ErrorType::Syntax, 
            t.pos,
            ErrorDisplayType::Error,
            self.lexer.filename.clone(),
            vec![
                ErrorAnnotation::new(Some("unexpected token".to_string()), t.pos, ErrorDisplayType::Error, self.lexer.filename.clone())
            ]
        )
    }

    /// Validate next token
    fn next(&mut self, token_type: lexer::TokenType, error_message: &str, position: (usize, usize), is_keyword: bool) -> Result<(), Error> {
        let t = self.lexer.advance().clone();
        
        if t.token != token_type {
            self.lexer.set_pos(position);
            Err(self.syntax_error(t, error_message, is_keyword))
        } else {
            Ok(())
        }
    }

    /// Parse basic block
    fn block(&mut self) -> Result<ast::Block, Error> {
        let position = self.lexer.get_pos();
        
        self.next(lexer::TokenType::LCP, "expected `{`", position, false)?;

        // Set our scope to inside a block
        let scope = Scope::Block;

        let mut ast_list: Vec<Statement> = Vec::new();
        loop {
            let mut errors: Vec<Error> = Vec::new();
            let mut fail = true;
            for i in 0..self.statements.len() {
                let statement_ast = self.statements[i](self);
                match statement_ast {
                    Ok(ast_production) => {
                        if ast_production.in_scope(&scope) {
                            fail = false; 
                            errors.clear();
                            ast_list.push(ast_production);
                            break
                        } else {
                            break
                        }
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
        self.next(lexer::TokenType::RCP, "expected `}`", position, false)?;

        Ok( ast::Block {
            nodes: ast_list,
            pos: helpers::Pos {
                s: position.0,
                e: self.lexer.position
            }
        } )
    }

    /// Parse function definition
    fn function_define(&mut self) -> Result<Statement, Error> {
        let position = self.lexer.get_pos();
        
        self.next(lexer::TokenType::DEF, "", position, true)?;

        let id = self.name_id()?;

        self.next(lexer::TokenType::LP, "expected `(`", position, false)?;

        let arguments = self.parse_arguments()?;

        self.next(lexer::TokenType::RP, "expected `)`", position, false)?;

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
        
        Ok(ast::Statement::FunctionDefine(ast::FunctionDefine {
            return_type,
            arguments,
            block,
            name: id,
            pos: helpers::Pos {
                s: position.0,
                e: self.lexer.position
            }
        }))
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

            self.next(lexer::TokenType::COLON, "expected `:`", position, false)?;

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
    fn expression_statement(&mut self) -> Result<Statement, Error> {
        let position = self.lexer.get_pos();

        let expr = self.expr()?;

        self.next(lexer::TokenType::SEMI, "expected `;`", position, false)?;

        Ok(ast::Statement::ExpressionStatement(ast::ExpressionStatement {
            expression: Box::new(expr),
            pos: helpers::Pos {
                s: position.0,
                e: self.lexer.position
            }
        }))
    }

    /// Ful variable assign with type declaration and expression
    fn variable_assign_full(&mut self) -> Result<Expr, Error> {
        let position = self.lexer.get_pos();
        self.next(lexer::TokenType::LET, "expected `let` keyword", position, true)?;

        let var_name = self.name_id()?;

        self.next(lexer::TokenType::COLON, "expected `:`", position, false)?;
        
        let var_type = self.type_expr()?;

        self.next(lexer::TokenType::EQUALS, "expected `=`", position, false)?;
        
        let expr = self.expr()?;

        Ok(ast::Expr::VariableAssignDeclaration(ast::VariableAssignDeclaration {
            t: var_type,
            name: var_name,
            expr: Box::new(expr),
            pos: helpers::Pos {
                s: position.0,
                e: self.lexer.position
            }
        }))
    }

    /// Variable Declaration
    fn variable_declaration(&mut self) -> Result<Statement, Error> {
        let position = self.lexer.get_pos();
        self.next(lexer::TokenType::LET, "expected `let` keyword", position, true)?;

        let var_name = self.name_id()?;

        self.next(lexer::TokenType::COLON, "expected `:`", position, false)?;
        
        let var_type = self.type_expr()?;

        self.next(lexer::TokenType::SEMI, "expected `;`", position, false)?;

        Ok(ast::Statement::VariableDeclaration(ast::VariableDeclaration {
            t: var_type,
            name: var_name,
            pos: helpers::Pos {
                s: position.0,
                e: self.lexer.position
            }
        }))
    }

    /// Variable assign with only expression
    fn variable_assign(&mut self) -> Result<Expr, Error> {
        let position = self.lexer.get_pos();

        let var_name = self.name_id()?;

        self.next(lexer::TokenType::EQUALS, "expected `=`", position, false)?;
        
        let expr = self.expr()?;

        Ok(ast::Expr::VariableAssign(ast::VariableAssign {
            name: var_name,
            expr: Box::new(expr),
            pos: helpers::Pos {
                s: position.0,
                e: self.lexer.position
            }
        }))
    }

    /// Top level expression
    fn expr(&mut self) -> Result<Expr, Error> {
        let position = self.lexer.get_pos();

        let mut left = self.term()?;

        loop {
            match self.lexer.peek().token {
                lexer::TokenType::ADD  => { 
                    self.lexer.advance();
                    let right = self.term()?;

                    left = ast::Expr::Add(ast::Add {
                        left: Box::new(left),
                        right: Box::new(right),
                        pos: helpers::Pos {
                            s: position.0,
                            e: self.lexer.position,
                        }
                    });
                },

                lexer::TokenType::SUB => {
                    self.lexer.advance();
                    let right = self.term()?;
                    
                    left = ast::Expr::Sub(ast::Sub {
                        left: Box::new(left),
                        right: Box::new(right),
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

    fn term(&mut self) -> Result<Expr, Error> {
        let position = self.lexer.get_pos();
        
        let mut left = self.factor()?;
        
        loop {
            match self.lexer.peek().token {
                lexer::TokenType::MUL  => { 
                    self.lexer.advance();
                    let right = self.factor()?;
                    
                    left = ast::Expr::Mul(ast::Mul {
                        left: Box::new(left),
                        right: Box::new(right),
                        pos: helpers::Pos {
                            s: position.0,
                            e: self.lexer.position,
                        }
                    });
                },

                lexer::TokenType::DIV => {
                    self.lexer.advance();
                    let right = self.factor()?;
                    
                    left = ast::Expr::Div(ast::Div {
                        left: Box::new(left),
                        right: Box::new(right),
                        pos: helpers::Pos {
                            s: position.0,
                            e: self.lexer.position,
                        }
                    });
                },

                lexer::TokenType::MOD  => {
                    self.lexer.advance();
                    let right = self.factor()?;
                    
                    left = ast::Expr::Mod(ast::Mod {
                        left: Box::new(left),
                        right: Box::new(right),
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

    fn factor(&mut self) -> Result<Expr, Error> {
        let position = self.lexer.get_pos();
        
        match self.next(lexer::TokenType::SUB, "expected `-`", position, false) {
            Ok(_) => {
                match self.item() {
                    Ok(item) => Ok(ast::Expr::Neg(
                        ast::Neg { 
                            value: Box::new(item), 
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

    fn item(&mut self) -> Result<Expr, Error> {
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
            return Err(self.syntax_error(next_tok, "expected `)`", false));
        }

        self.lexer.set_pos(position);
        let next_tok = self.lexer.peek().clone();
        Err(self.syntax_error(next_tok, "expected expression", false))
    }

    fn integer(&mut self) -> Result<Expr, Error> {
        let position = self.lexer.get_pos();

        let int = self.lexer.advance().clone();
        if let lexer::TokenType::NUMBER(value) = &int.token {
            Ok(
                ast::Expr::Integer(ast::Integer {
                    value: value.to_string(),
                    pos: int.pos
                })
            )
        } else {
            self.lexer.set_pos(position);
            Err(self.syntax_error(int.clone(), "expected integer", false))
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
            Err(self.syntax_error(id.clone(), "expected identifier", false))
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
        Err(self.syntax_error(id.clone(), "expected identifier", false))
    }

}
