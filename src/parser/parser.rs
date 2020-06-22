use crate::helpers;
use crate::lexer;
use crate::logger::logger::{Error, ErrorAnnotation, ErrorDisplayType, ErrorType, Logger};
use crate::parser::ast;
use crate::parser::ast::{Expr, Scope, Statement, TypeCheckOrType};
use crate::paths;
use crate::typecheck::ast_typecheck::TypeCheckType;

use std::cell::RefCell;
use std::collections::HashMap;
use std::path;
use std::rc::Rc;

macro_rules! ignore_or_return {
    ( $e:expr ) => {
        match $e {
            Ok(x) => return Ok(x),
            Err(e) => {
                if e.is_priority() {
                    return Err(e);
                }
            }
        }
    };
}

#[derive(Copy, Clone, PartialEq, PartialOrd)]
pub enum Prec {
    LOWEST = 0,

    /// Comparison operators
    COMP = 1,

    /// `+` and `-`
    TERM = 2,

    /// `*` and `/` and `%`
    FACTOR = 3,

    /// `as` operator (CONVersion operator)
    CONV = 4,

    /// `-` (negate) and others (i.e. `!` logical negate)
    PREFIX = 5,
}

/// Recursive descent parser
pub struct Parser<'a> {
    /// Lexer object
    pub lexer: lexer::Lexer<'a>,
    /// Abstract syntax tree
    pub ast: Option<ast::Block<'a>>,
    statements: [fn(&mut Self) -> Result<Statement<'a>, Error<'a>>; 12],
    prefix_op: HashMap<lexer::TokenType<'a>, Prec>,
    infix_op: HashMap<lexer::TokenType<'a>, Prec>,
    tokens: Vec<lexer::Token<'a>>,
    pub logger: Rc<RefCell<Logger<'a>>>,
    token_pos: usize,
}

impl<'a> Parser<'a> {
    /// Return a new parser object.
    pub fn new(
        filename: &'a path::Path,
        file_contents: &'a str,
        logger: Rc<RefCell<Logger<'a>>>,
    ) -> Parser<'a> {
        let l = lexer::Lexer::new(filename, file_contents);
        Parser {
            lexer: l,
            ast: None,
            statements: [
                Parser::import,
                Parser::unit,
                Parser::function_define,
                Parser::expression_statement,
                Parser::return_statement,
                Parser::variable_declaration,
                Parser::type_assign,
                Parser::compiler_tag,
                Parser::extern_def,
                Parser::conditional,
                Parser::overload_define,
                Parser::overload_extern,
            ],
            prefix_op: HashMap::new(),
            infix_op: HashMap::new(),
            tokens: Vec::new(),
            logger,
            token_pos: 0,
        }
    }
    /// Template for syntax error
    pub fn syntax_error(
        &self,
        t: lexer::Token<'a>,
        message: &str,
        is_keyword: bool,
        urgent: bool,
    ) -> Error<'a> {
        Error::new(
            if !is_keyword {
                format!("{}, found {}", message, t)
            } else {
                format!("unexpected {}", t)
            },
            ErrorType::Syntax,
            t.pos,
            ErrorDisplayType::Error,
            vec![ErrorAnnotation::new(
                Some("unexpected token".to_string()),
                t.pos,
                ErrorDisplayType::Error,
            )],
            urgent,
        )
    }

    /// Validate next token
    pub fn next(
        &mut self,
        token_type: lexer::TokenType<'_>,
        position: usize,
        is_keyword: bool,
    ) -> Result<(), Error<'a>> {
        let prev_pos = self.token_pos;
        let t = self.forward();

        if t.token != token_type {
            let error = format!("expected {}", token_type);
            let temp = match token_type {
                lexer::TokenType::SEMI => {
                    // Special case for semicolon error
                    let mut semi_error = self.syntax_error(t, &error[..], is_keyword, false);
                    let position_e = self.tokens[prev_pos - 1].pos.e;
                    semi_error.annotations.push(ErrorAnnotation::new(
                        Some("did you mean to put a `;` here?".to_string()),
                        helpers::Pos::new(position_e - 1, position_e, self.lexer.filename),
                        ErrorDisplayType::Info,
                    ));
                    Err(semi_error)
                }
                _ => Err(self.syntax_error(t, &error[..], is_keyword, false)),
            };

            self.set_pos(position);
            temp
        } else {
            Ok(())
        }
    }

    pub fn get_relative_pos(&mut self, position: usize) -> helpers::Pos<'a> {
        helpers::Pos {
            s: self.tokens[position].pos.s,
            e: self.tokens[if self.token_pos > 0 {
                self.token_pos - 1
            } else {
                self.token_pos
            }]
            .pos
            .e,
            filename: self.lexer.filename,
        }
    }

    pub fn initialize_expr(&mut self) {
        self.register_prefix(lexer::TokenType::SUB, Prec::PREFIX);

        // `-`
        self.register_infix(lexer::TokenType::SUB, Prec::TERM);
        // `+`
        self.register_infix(lexer::TokenType::ADD, Prec::TERM);

        // `/`
        self.register_infix(lexer::TokenType::DIV, Prec::FACTOR);
        // `%`
        self.register_infix(lexer::TokenType::MOD, Prec::FACTOR);
        // `*`
        self.register_infix(lexer::TokenType::MUL, Prec::FACTOR);
        // `%%`
        self.register_infix(lexer::TokenType::DMOD, Prec::FACTOR);

        // `as` keyword
        self.register_infix(lexer::TokenType::AS, Prec::CONV);

        // `>`
        self.register_infix(lexer::TokenType::GT, Prec::COMP);
        // `<`
        self.register_infix(lexer::TokenType::LT, Prec::COMP);
        // `>=`
        self.register_infix(lexer::TokenType::GE, Prec::COMP);
        // `<=`
        self.register_infix(lexer::TokenType::LE, Prec::COMP);
        // `==`
        self.register_infix(lexer::TokenType::EQ, Prec::COMP);
    }

    pub fn register_prefix(&mut self, token: lexer::TokenType<'a>, prec: Prec) {
        self.prefix_op.insert(token, prec);
    }

    pub fn register_infix(&mut self, token: lexer::TokenType<'a>, prec: Prec) {
        self.infix_op.insert(token, prec);
    }

    fn fill_token_stream(&mut self) -> Result<(), Error<'a>> {
        loop {
            let next = self.lexer.advance();
            match next {
                Ok(val) => {
                    self.tokens.push(val);
                    if val.token == lexer::TokenType::EOF {
                        break;
                    }
                }
                Err(e) => return Err(e),
            }
        }
        Ok(())
    }

    fn peek(&self) -> lexer::Token<'a> {
        self.tokens[self.token_pos]
    }

    fn forward(&mut self) -> lexer::Token<'a> {
        let temp = self.tokens[self.token_pos];
        self.token_pos += 1;
        temp
    }

    fn set_pos(&mut self, pos: usize) {
        self.token_pos = pos;
    }

    /// Parse from lexer tokens
    ///
    /// Returns nothing
    pub fn parse(&mut self) -> Result<(), Vec<Error<'a>>> {
        if let Err(e) = self.fill_token_stream() {
            return Err(vec![e]);
        }

        // Set our scope to outside
        let scope = Scope::Outer;
        let position = self.token_pos;

        let mut ast_list: Vec<Statement<'_>> = Vec::new();
        let next = self.peek();
        if next.token != lexer::TokenType::EOF {
            loop {
                let mut errors: Vec<Error<'_>> = Vec::new();
                let mut fail = true;
                for i in 0..self.statements.len() {
                    let statement_ast = self.statements[i](self);
                    match statement_ast {
                        Ok(ast_production) => {
                            if ast_production.in_scope(&scope) {
                                fail = false;
                                errors.clear();
                                ast_list.push(ast_production);
                                break;
                            } else {
                                errors.push(Error::new(
                                    "unexpected statement in outer scope".to_string(),
                                    ErrorType::Syntax,
                                    ast_production.pos(),
                                    ErrorDisplayType::Error,
                                    vec![ErrorAnnotation::new(
                                        Some("unexpected statement".to_string()),
                                        ast_production.pos(),
                                        ErrorDisplayType::Error,
                                    )],
                                    true,
                                ));
                                break;
                            }
                        }
                        Err(e) => {
                            if e.is_priority() {
                                return Err(vec![e]);
                            }
                            errors.push(e);
                        }
                    }
                }
                let temp_peek = self.peek();
                if temp_peek.token == lexer::TokenType::EOF && !fail {
                    // We've successfully parsed, break
                    break;
                } else if !errors.is_empty() && fail {
                    // We've found an error, raise the error
                    return Err(vec![Logger::longest(errors)]);
                }
            }
        }

        let block = ast::Block {
            nodes: ast_list,
            tags: Vec::new(),
            pos: self.get_relative_pos(position),
            insert_return: false,
        };
        self.ast = Some(block);
        Ok(())
    }

    /// Parse basic block
    pub fn block(&mut self, scope: Scope) -> Result<ast::Block<'a>, Error<'a>> {
        let position = self.token_pos;
        self.next(lexer::TokenType::LCP, position, false)?;

        let mut ast_list: Vec<Statement<'_>> = Vec::new();
        loop {
            let mut errors: Vec<Error<'_>> = Vec::new();
            let mut fail = true;
            for i in 0..self.statements.len() {
                let statement_ast = self.statements[i](self);
                match statement_ast {
                    Ok(ast_production) => {
                        if ast_production.in_scope(&scope) {
                            fail = false;
                            errors.clear();
                            ast_list.push(ast_production);
                            break;
                        } else {
                            errors.push(Error::new(
                                "unexpected statement in inner scope".to_string(),
                                ErrorType::Syntax,
                                ast_production.pos(),
                                ErrorDisplayType::Error,
                                vec![ErrorAnnotation::new(
                                    Some("unexpected statement".to_string()),
                                    ast_production.pos(),
                                    ErrorDisplayType::Error,
                                )],
                                true,
                            ));
                            break;
                        }
                    }
                    Err(e) => {
                        errors.push(e);
                    }
                }
            }

            if self.peek().token == lexer::TokenType::RCP {
                // We've successfully parsed, break
                break;
            } else if !errors.is_empty() && fail {
                // We've found an error, return the error
                return Err(Logger::longest(errors));
            } else if self.peek().token != lexer::TokenType::RCP && fail {
                // We've forgotten closing brace
                break;
            }
        }

        // Check for closing brace here
        self.next(lexer::TokenType::RCP, position, false)?;

        Ok(ast::Block {
            nodes: ast_list,
            tags: Vec::new(),
            pos: self.get_relative_pos(position),
            insert_return: false,
        })
    }

    fn parse_arguments(&mut self) -> Result<ast::Arguments<'a>, Error<'a>> {
        let position = self.token_pos;
        let mut positional_args: Vec<(ast::NameID<'_>, ast::TypeCheckOrType<'_>)> = Vec::new();

        loop {
            if self.peek().token == lexer::TokenType::RP {
                // No error, we've reached the end
                break;
            }

            let id = self.name_id()?;

            self.next(lexer::TokenType::COLON, position, false)?;

            let arg_type = self.type_expr()?;

            positional_args.push((id, ast::TypeCheckOrType::Type(Rc::new(arg_type))));
            if self.peek().token == lexer::TokenType::COMMA {
                self.forward();
            } else {
                break;
            }
        }

        Ok(ast::Arguments {
            positional: positional_args,
            pos: self.get_relative_pos(position),
        })
    }

    fn type_assign(&mut self) -> Result<Statement<'a>, Error<'a>> {
        let position = self.token_pos;

        let visibility = if self.peek().token == lexer::TokenType::PUBLIC {
            self.forward();
            ast::Visibility::Public
        } else {
            ast::Visibility::Private
        };
        self.next(lexer::TokenType::TYPE, position, true)?;

        let name = self.namespace()?;

        self.next(lexer::TokenType::EQUALS, position, false)?;

        let value = self.type_expr()?;

        self.next(lexer::TokenType::SEMI, position, false)?;

        Ok(Statement::TypeAssign(ast::TypeAssign {
            value: ast::TypeCheckOrType::Type(Rc::new(value)),
            name: Rc::new(name),
            visibility,
            pos: self.get_relative_pos(position),
        }))
    }

    fn unit(&mut self) -> Result<Statement<'a>, Error<'a>> {
        let position = self.token_pos;
        self.next(lexer::TokenType::UNIT, position, true)?;
        let name = self.namespace()?;

        let block = self.block(Scope::Outer)?;

        Ok(Statement::Unit(ast::Unit {
            name,
            block,
            pos: self.get_relative_pos(position),
        }))
    }

    fn add_file(&mut self, name: &mut ast::Namespace<'a>) -> Result<Statement<'a>, Error<'a>> {
        let mut scopes = Vec::new();
        std::mem::swap(&mut scopes, &mut name.scopes);

        let mut import_path = path::PathBuf::new();
        import_path.push(self.lexer.filename);
        import_path = paths::get_parent(paths::canonicalize_file(&import_path));

        let mut last_idx: usize = 0;
        for (idx, op) in scopes.iter().enumerate() {
            if import_path.join(format!("{}.fl", op.value)).is_file() {
                import_path.push(format!("{}.fl", op.value));
                // We've reached a file, break
                last_idx = idx + 1;
                break;
            } else if import_path.join(op.value).is_dir() {
                // We've reached a directory
                import_path.push(op.value);
            } else {
                // Its not a file or directory, break
                last_idx = idx + 1;
                break;
            }
        }

        let last = scopes.drain(0..last_idx).into_iter().last().unwrap();

        import_path = paths::canonicalize_file(&import_path)
            .strip_prefix(paths::canonicalize_file(&std::env::current_dir().unwrap()))
            .unwrap()
            .to_path_buf();

        if !import_path.is_file() {
            Err(Error::new(
                "file does not exist".to_string(),
                ErrorType::ImportError,
                last.pos,
                ErrorDisplayType::Error,
                vec![ErrorAnnotation::new(
                    Some(format!("file `{}` does not exist", {
                        import_path.push(format!("{}.fl", last.value));
                        import_path.display()
                    })),
                    last.pos,
                    ErrorDisplayType::Error,
                )],
                true,
            ))
        } else if scopes.is_empty() {
            // We can leak because the memory is going to live for most the the program anyways
            let imported_filename: &'static path::Path = path::Path::new(Box::leak(
                import_path.to_string_lossy().into_owned().into_boxed_str(),
            ));
            let file_contents = paths::read_file_leak(imported_filename);
            self.logger
                .borrow_mut()
                .add_file(imported_filename, file_contents);
            let mut parser = Parser::new(imported_filename, file_contents, Rc::clone(&self.logger));
            parser.initialize_expr();
            match parser.parse() {
                Ok(_) => {}
                Err(e) => {
                    let mut error = e.into_iter().nth(0).unwrap();
                    error.urgent = true;
                    return Err(error);
                }
            };

            Ok(Statement::Unit(ast::Unit {
                pos: last.pos,
                name: last.into_namespace(),
                block: parser.ast.unwrap(),
            }))
        } else {
            Err(Error::new(
                "cannot declare a unit any further than file".to_string(),
                ErrorType::ImportError,
                name.pos,
                ErrorDisplayType::Error,
                vec![
                    ErrorAnnotation::new(
                        Some(format!(
                            "cannot declare unit more from `{}`",
                            import_path.display()
                        )),
                        name.pos,
                        ErrorDisplayType::Error,
                    ),
                    ErrorAnnotation::new(
                        Some("help: did you mean to alias?".to_string()),
                        name.pos,
                        ErrorDisplayType::Info,
                    ),
                ],
                true,
            ))
        }
    }

    /// Imports
    fn import(&mut self) -> Result<Statement<'a>, Error<'a>> {
        let position = self.token_pos;
        self.next(lexer::TokenType::UNIT, position, true)?;

        let mut namespace = self.namespace()?;

        self.next(lexer::TokenType::SEMI, position, false)?;

        self.add_file(&mut namespace)
    }

    fn compiler_tag(&mut self) -> Result<Statement<'a>, Error<'a>> {
        let position = self.token_pos;

        self.next(lexer::TokenType::AT, position, true)?;
        self.next(lexer::TokenType::LB, position, false)?;

        // For now, tags can only be ids
        let id = self.name_id()?;

        self.next(lexer::TokenType::RB, position, false)?;

        Ok(Statement::Tag(ast::Tag {
            content: id,
            pos: self.get_relative_pos(position),
        }))
    }

    /// Parse function definition
    fn function_define(&mut self) -> Result<Statement<'a>, Error<'a>> {
        let position = self.token_pos;

        let visibility = if self.peek().token == lexer::TokenType::PUBLIC {
            self.forward();
            ast::Visibility::Public
        } else {
            ast::Visibility::Private
        };

        self.next(lexer::TokenType::DEF, position, true)?;
        let name = Rc::new(self.namespace()?);

        self.next(lexer::TokenType::LP, position, false)?;

        let arguments = self.parse_arguments()?;

        self.next(lexer::TokenType::RP, position, false)?;

        let block;
        let return_type: ast::Type<'_> = if self.peek().token == lexer::TokenType::ARROW {
            self.forward();
            let temp = self.type_expr()?;
            block = self.block(Scope::Block)?;
            temp
        } else if {
            block = self.block(Scope::Block)?;
            true
        } {
            ast::Type {
                value: ast::TypeType::Tuple(Vec::new()),
                inferred: true,
                pos: block.pos,
            }
        } else {
            ast::Type {
                value: ast::TypeType::Tuple(Vec::new()),
                inferred: true,
                pos: block.pos,
            }
        };

        Ok(ast::Statement::FunctionDefine(ast::FunctionDefine {
            return_type: ast::TypeCheckOrType::Type(Rc::new(return_type)),
            arguments,
            block: Some(block),
            visibility,
            name,
            mangled_name: None,
            pos: self.get_relative_pos(position),
            overload_operator: None,
        }))
    }

    fn extern_def(&mut self) -> Result<Statement<'a>, Error<'a>> {
        let position = self.token_pos;

        let visibility = if self.peek().token == lexer::TokenType::PUBLIC {
            self.forward();
            ast::Visibility::Public
        } else {
            ast::Visibility::Private
        };

        self.next(lexer::TokenType::EXTERN, position, true)?;

        self.next(lexer::TokenType::DEF, position, true)?;
        let name = Rc::new(self.namespace()?);

        self.next(lexer::TokenType::LP, position, false)?;

        let arguments = self.parse_extern_arguments()?;

        self.next(lexer::TokenType::RP, position, false)?;

        let return_type: ast::Type<'_> = if self.peek().token == lexer::TokenType::ARROW {
            self.forward();
            self.type_expr()?
        } else {
            self.forward();
            ast::Type {
                value: ast::TypeType::Tuple(Vec::new()),
                inferred: true,
                pos: self.get_relative_pos(position),
            }
        };

        self.next(lexer::TokenType::SEMI, position, false)?;

        Ok(ast::Statement::FunctionDefine(ast::FunctionDefine {
            return_type: ast::TypeCheckOrType::Type(Rc::new(return_type)),
            arguments,
            visibility,
            name,
            mangled_name: None,
            pos: self.get_relative_pos(position),
            block: None, // No block on external function
            overload_operator: None,
        }))
    }

    fn overload_define(&mut self) -> Result<Statement<'a>, Error<'a>> {
        let position = self.token_pos;

        let visibility = if self.peek().token == lexer::TokenType::PUBLIC {
            self.forward();
            ast::Visibility::Public
        } else {
            ast::Visibility::Private
        };

        self.next(lexer::TokenType::OVERLOAD, position, true)?;

        let overload_operator = Some(self.forward().token);
        let name = Rc::new(self.namespace()?);

        self.next(lexer::TokenType::LP, position, false)?;

        let arguments = self.parse_arguments()?;

        self.next(lexer::TokenType::RP, position, false)?;

        let block;
        let return_type: ast::Type<'_> = if self.peek().token == lexer::TokenType::ARROW {
            self.forward();
            let temp = self.type_expr()?;
            block = self.block(Scope::Block)?;
            temp
        } else if {
            block = self.block(Scope::Block)?;
            true
        } {
            ast::Type {
                value: ast::TypeType::Tuple(Vec::new()),
                inferred: true,
                pos: block.pos,
            }
        } else {
            ast::Type {
                value: ast::TypeType::Tuple(Vec::new()),
                inferred: true,
                pos: block.pos,
            }
        };

        Ok(ast::Statement::FunctionDefine(ast::FunctionDefine {
            return_type: ast::TypeCheckOrType::Type(Rc::new(return_type)),
            arguments,
            block: Some(block),
            visibility,
            name,
            mangled_name: None,
            pos: self.get_relative_pos(position),
            overload_operator,
        }))
    }

    fn overload_extern(&mut self) -> Result<Statement<'a>, Error<'a>> {
        let position = self.token_pos;

        let visibility = if self.peek().token == lexer::TokenType::PUBLIC {
            self.forward();
            ast::Visibility::Public
        } else {
            ast::Visibility::Private
        };

        self.next(lexer::TokenType::EXTERN, position, true)?;

        self.next(lexer::TokenType::OVERLOAD, position, true)?;

        let overload_operator = Some(self.forward().token);
        let name = Rc::new(self.namespace()?);

        self.next(lexer::TokenType::LP, position, false)?;

        let arguments = self.parse_extern_arguments()?;

        self.next(lexer::TokenType::RP, position, false)?;

        let return_type: ast::Type<'_> = if self.peek().token == lexer::TokenType::ARROW {
            self.forward();
            self.type_expr()?
        } else {
            self.forward();
            ast::Type {
                value: ast::TypeType::Tuple(Vec::new()),
                inferred: true,
                pos: self.get_relative_pos(position),
            }
        };

        self.next(lexer::TokenType::SEMI, position, false)?;

        Ok(ast::Statement::FunctionDefine(ast::FunctionDefine {
            return_type: ast::TypeCheckOrType::Type(Rc::new(return_type)),
            arguments,
            visibility,
            name,
            mangled_name: None,
            pos: self.get_relative_pos(position),
            block: None, // No block on external function
            overload_operator,
        }))
    }

    fn parse_extern_arguments(&mut self) -> Result<ast::Arguments<'a>, Error<'a>> {
        let position = self.token_pos;
        let mut positional_args: Vec<(ast::NameID<'_>, ast::TypeCheckOrType<'_>)> = Vec::new();

        loop {
            if self.peek().token == lexer::TokenType::RP {
                // No error, we've reached the end
                break;
            }

            let position = self.token_pos;
            let id = self.name_id();

            if let (Ok(id_val), lexer::TokenType::COLON) = (&id, self.peek().token) {
                // There is a name id, eat it; its optional
                self.forward();

                let arg_type = self.type_expr()?;
                positional_args.push((*id_val, ast::TypeCheckOrType::Type(Rc::new(arg_type))));
            } else {
                // No name id, its optional so this is fine
                self.token_pos = position;
                let arg_type = self.type_expr()?;
                positional_args.push((
                    ast::NameID {
                        value: "none",
                        pos: self.get_relative_pos(position),
                    },
                    ast::TypeCheckOrType::Type(Rc::new(arg_type)),
                ));
            }

            if self.peek().token == lexer::TokenType::COMMA {
                self.forward();
            } else {
                break;
            }
        }

        Ok(ast::Arguments {
            positional: positional_args,
            pos: self.get_relative_pos(position),
        })
    }

    /// Return statement
    fn return_statement(&mut self) -> Result<Statement<'a>, Error<'a>> {
        let position = self.token_pos;

        self.next(lexer::TokenType::RETURN, position, true)?;

        let expr = self.expr(Prec::LOWEST)?;
        self.next(lexer::TokenType::SEMI, position, false)?;

        Ok(ast::Statement::Return(ast::Return {
            expression: Box::new(expr),
            pos: self.get_relative_pos(position),
        }))
    }

    /// Expressions statement
    fn expression_statement(&mut self) -> Result<Statement<'a>, Error<'a>> {
        let position = self.token_pos;
        let expr = self.expr(Prec::LOWEST)?;

        self.next(lexer::TokenType::SEMI, position, false)?;

        Ok(ast::Statement::ExpressionStatement(
            ast::ExpressionStatement {
                expression: Box::new(expr),
                pos: self.get_relative_pos(position),
            },
        ))
    }

    /// Arguments of function call
    fn arguments_call(&mut self) -> Result<ast::ArgumentsRun<'a>, Error<'a>> {
        let position = self.token_pos;
        let mut positional_args: Vec<Expr<'_>> = Vec::new();

        loop {
            if self.peek().token == lexer::TokenType::RP {
                // No error, we've reached the end
                break;
            }

            let expr = self.expr(Prec::LOWEST)?;

            positional_args.push(expr);
            if self.peek().token == lexer::TokenType::COMMA {
                self.forward();
            } else {
                break;
            }
        }

        Ok(ast::ArgumentsRun {
            positional: positional_args,
            pos: self.get_relative_pos(position),
        })
    }

    /// Function calls
    fn function_call(&mut self) -> Result<Expr<'a>, Error<'a>> {
        let position = self.token_pos;

        let namespace = self.namespace()?;

        self.next(lexer::TokenType::LP, position, false)?;

        let arguments = self.arguments_call()?;

        self.next(lexer::TokenType::RP, position, false)?;

        Ok(ast::Expr::FunctionCall(ast::FunctionCall {
            arguments,
            name: Rc::new(namespace),
            pos: self.get_relative_pos(position),
            mangled_name: None,
            mangle: true,
        }))
    }

    /// Ful variable assign with type declaration and expression
    fn variable_assign_full(&mut self) -> Result<Expr<'a>, Error<'a>> {
        let position = self.token_pos;
        self.next(lexer::TokenType::LET, position, true)?;

        let namespace = self.namespace()?;

        self.next(lexer::TokenType::COLON, position, false)?;
        let var_type = self.type_expr()?;

        self.next(lexer::TokenType::EQUALS, position, false)?;
        let expr = self.expr(Prec::LOWEST)?;

        Ok(ast::Expr::VariableAssignDeclaration(
            ast::VariableAssignDeclaration {
                t: ast::TypeCheckOrType::Type(Rc::new(var_type)),
                name: Rc::new(namespace),
                expr: Box::new(expr),
                pos: self.get_relative_pos(position),
            },
        ))
    }

    /// Variable Declaration
    fn variable_declaration(&mut self) -> Result<Statement<'a>, Error<'a>> {
        let position = self.token_pos;
        self.next(lexer::TokenType::LET, position, true)?;

        let namespace = self.namespace()?;

        self.next(lexer::TokenType::COLON, position, false)?;
        let var_type = self.type_expr()?;

        self.next(lexer::TokenType::SEMI, position, false)?;

        Ok(ast::Statement::VariableDeclaration(
            ast::VariableDeclaration {
                t: ast::TypeCheckOrType::Type(Rc::new(var_type)),
                name: Rc::new(namespace),
                pos: self.get_relative_pos(position),
            },
        ))
    }

    /// Variable assign with only expression
    fn variable_assign(&mut self) -> Result<Expr<'a>, Error<'a>> {
        let position = self.token_pos;

        let namespace = self.namespace()?;

        self.next(lexer::TokenType::EQUALS, position, false)?;

        let expr = self.expr(Prec::LOWEST)?;

        Ok(ast::Expr::VariableAssign(ast::VariableAssign {
            name: Rc::new(namespace),
            expr: Box::new(expr),
            type_val: None,
            pos: self.get_relative_pos(position),
        }))
    }

    fn conditional(&mut self) -> Result<Statement<'a>, Error<'a>> {
        let position = self.token_pos;

        // Required
        let mut if_branches = vec![self.if_cond()?];

        loop {
            match self.else_if_cond() {
                Ok(val) => if_branches.push(val),
                Err(_) => break,
            }
        }

        // Optional
        let else_branch = match self.else_cond() {
            Ok(val) => Some(val),
            Err(_) => None,
        };

        Ok(ast::Statement::Conditional(ast::Conditional {
            if_branches,
            else_branch,
            pos: self.get_relative_pos(position),
        }))
    }

    fn if_cond(&mut self) -> Result<ast::IfBranch<'a>, Error<'a>> {
        let position = self.token_pos;

        self.next(lexer::TokenType::IF, position, true)?;
        self.next(lexer::TokenType::LP, position, false)?;

        let cond = self.expr(Prec::LOWEST)?;

        self.next(lexer::TokenType::RP, position, false)?;

        let block = self.block(Scope::Block)?;

        Ok(ast::IfBranch {
            cond,
            block,
            pos: self.get_relative_pos(position),
        })
    }

    fn else_if_cond(&mut self) -> Result<ast::IfBranch<'a>, Error<'a>> {
        let position = self.token_pos;

        self.next(lexer::TokenType::ELSE, position, true)?;
        self.next(lexer::TokenType::IF, position, true)?;
        self.next(lexer::TokenType::LP, position, false)?;

        let cond = self.expr(Prec::LOWEST)?;

        self.next(lexer::TokenType::RP, position, false)?;

        let block = self.block(Scope::Block)?;

        Ok(ast::IfBranch {
            cond,
            block,
            pos: self.get_relative_pos(position),
        })
    }

    fn else_cond(&mut self) -> Result<ast::ElseBranch<'a>, Error<'a>> {
        let position = self.token_pos;

        self.next(lexer::TokenType::ELSE, position, true)?;
        let block = self.block(Scope::Block)?;

        Ok(ast::ElseBranch {
            block,
            pos: self.get_relative_pos(position),
        })
    }

    /// Top level expression
    fn expr(&mut self, prec: Prec) -> Result<Expr<'a>, Error<'a>> {
        let mut left = self.item()?;
        let mut operator;

        loop {
            match self.get_operator_infix() {
                Ok(op) => {
                    operator = op;
                }
                Err(_) => {
                    return Ok(left);
                }
            }
            let binding_power = self.binding_power(&operator.token) as u8;
            // Unmatched expression if Err(), raise error (priority mode)
            match self.led(left, operator) {
                Ok(val) => {
                    left = val;
                }
                Err(mut e) => {
                    e.urgent = true;
                    e.position.e += 1;
                    return Err(e);
                }
            }
            if binding_power > prec as u8 {
                break;
            }
        }

        Ok(left)
    }

    fn get_operator_infix(&mut self) -> Result<lexer::Token<'a>, Error<'a>> {
        let position = self.token_pos;
        let potential_op = self.forward();
        for op in self.infix_op.keys() {
            if &potential_op.token == op {
                return Ok(potential_op);
            }
        }
        let temp = Err(Error::new(
            "Expected an operator".to_string(),
            ErrorType::Syntax,
            self.get_relative_pos(position),
            ErrorDisplayType::Error,
            vec![ErrorAnnotation::new(
                None,
                self.get_relative_pos(position),
                ErrorDisplayType::Error,
            )],
            false,
        ));

        self.set_pos(position);
        temp
    }

    fn get_operator_prefix(&mut self) -> Result<lexer::Token<'a>, Error<'a>> {
        let position = self.token_pos;
        let potential_op = self.forward();
        for op in self.prefix_op.keys() {
            if &potential_op.token == op {
                return Ok(potential_op);
            }
        }

        let temp = Err(Error::new(
            "Expected a prefix".to_string(),
            ErrorType::Syntax,
            self.get_relative_pos(position),
            ErrorDisplayType::Error,
            vec![ErrorAnnotation::new(
                None,
                self.get_relative_pos(position),
                ErrorDisplayType::Error,
            )],
            false,
        ));

        self.set_pos(position);
        temp
    }

    fn binding_power(&mut self, token_type: &lexer::TokenType<'a>) -> Prec {
        self.infix_op[token_type]
    }

    fn led(&mut self, left: Expr<'a>, operator: lexer::Token<'a>) -> Result<Expr<'a>, Error<'a>> {
        let position = self.token_pos;
        let binding_power = self.binding_power(&operator.token);

        Ok(Expr::Infix(ast::Infix {
            left: Box::new(left),
            operator,
            right: Box::new(self.expr(binding_power)?),
            function_call: None,
            pos: self.get_relative_pos(position),
        }))
    }

    fn item(&mut self) -> Result<Expr<'a>, Error<'a>> {
        let position = self.token_pos;

        if let Ok(prefix) = self.get_operator_prefix() {
            let bp = self.binding_power(&prefix.token);
            let item = self.expr(bp)?;
            return Ok(Expr::Prefix(ast::Prefix {
                operator: prefix,
                val: Box::new(item),
                type_val: None,
                pos: self.get_relative_pos(position),
            }));
        }

        let exprs = [
            Parser::number_type,
            Parser::integer,
            Parser::string_literal,
            Parser::dollar_expr,
            Parser::function_call,
            Parser::variable_assign,
            Parser::variable_assign_full,
            Parser::ref_expr,
            Parser::tuple_expr,
            Parser::bool_expr,
        ];
        for expr in exprs.iter() {
            ignore_or_return!(expr(self));
        }

        if let lexer::TokenType::LP = self.forward().token {
            let expr = self.expr(Prec::LOWEST)?;
            if let lexer::TokenType::RP = self.forward().token {
                return Ok(expr);
            }
            let next_tok = self.peek();
            return Err(self.syntax_error(next_tok, "expected `)`", false, false));
        }

        let next = self.peek();
        let temp = Err(Error::new(
            "Missing expression".to_string(),
            ErrorType::Syntax,
            self.get_relative_pos(position),
            ErrorDisplayType::Error,
            vec![ErrorAnnotation::new(
                Some(format!("Expected expression, found {}", next)),
                self.get_relative_pos(position),
                ErrorDisplayType::Error,
            )],
            false,
        ));
        self.set_pos(position);
        temp
    }

    fn bool_expr(&mut self) -> Result<Expr<'a>, Error<'a>> {
        let position = self.token_pos;

        let possible_bool = self.forward();
        if let lexer::TokenType::BOOL(value) = &possible_bool.token {
            Ok(ast::Expr::Literal(ast::Literal {
                value,
                type_val: TypeCheckOrType::TypeCheckType(TypeCheckType::construct_basic(
                    "bool",
                    possible_bool.pos,
                )),
                pos: possible_bool.pos,
            }))
        } else {
            let temp = Err(self.syntax_error(possible_bool, "expected bool", false, false));
            self.set_pos(position);
            temp
        }
    }

    fn integer(&mut self) -> Result<Expr<'a>, Error<'a>> {
        let position = self.token_pos;

        let int = self.forward();
        if let lexer::TokenType::NUMBER(value) = &int.token {
            Ok(ast::Expr::Literal(ast::Literal {
                value,
                type_val: TypeCheckOrType::TypeCheckType(TypeCheckType::construct_basic(
                    "{number}", int.pos,
                )),
                pos: int.pos,
            }))
        } else {
            let temp = Err(self.syntax_error(int, "expected integer", false, false));
            self.set_pos(position);
            temp
        }
    }

    fn number_type(&mut self) -> Result<Expr<'a>, Error<'a>> {
        let position = self.token_pos;

        let next_tok = self.forward();
        let int = if let lexer::TokenType::NUMBER(value) = next_tok.token {
            value
        } else {
            let temp = Err(self.syntax_error(next_tok, "expected integer", false, false));
            self.set_pos(position);
            return temp;
        };

        let type_val = Rc::new(match self.type_expr() {
            Ok(val) => val,
            Err(e) => {
                self.set_pos(position);
                return Err(e);
            }
        });

        Ok(ast::Expr::Literal(ast::Literal {
            value: int,
            pos: helpers::Pos::new(next_tok.pos.s, type_val.pos.e, next_tok.pos.filename),
            type_val: TypeCheckOrType::Type(type_val),
        }))
    }

    fn string_literal(&mut self) -> Result<Expr<'a>, Error<'a>> {
        let position = self.token_pos;

        let string = self.forward();
        if let lexer::TokenType::STRING(value) = &string.token {
            Ok(ast::Expr::Literal(ast::Literal {
                value,
                type_val: TypeCheckOrType::TypeCheckType(TypeCheckType::construct_basic(
                    "str", string.pos,
                )),
                pos: string.pos,
            }))
        } else {
            let temp = Err(self.syntax_error(string, "expected string literal", false, false));
            self.set_pos(position);
            temp
        }
    }

    fn namespace(&mut self) -> Result<ast::Namespace<'a>, Error<'a>> {
        let position = self.token_pos;
        let mut ids: Vec<ast::NameID<'_>> = Vec::new();
        let id = self.name_id()?;

        ids.push(id);

        loop {
            if self.peek().token != lexer::TokenType::DOUBLECOLON {
                break;
            }

            self.next(lexer::TokenType::DOUBLECOLON, position, false)?;

            match self.name_id() {
                Ok(id) => {
                    ids.push(id);
                }
                Err(e) => {
                    self.set_pos(position);
                    return Err(e);
                }
            }
        }

        Ok(ast::Namespace {
            scopes: ids,
            pos: self.get_relative_pos(position),
        })
    }

    /// Parse name identifier (i.e. function name)
    fn name_id(&mut self) -> Result<ast::NameID<'a>, Error<'a>> {
        let position = self.token_pos;
        let id = self.forward();
        if let lexer::TokenType::IDENTIFIER(value) = &id.token {
            Ok(ast::NameID { value, pos: id.pos })
        } else {
            let temp = Err(self.syntax_error(id, "expected identifier", false, false));
            self.set_pos(position);
            temp
        }
    }

    fn ref_expr(&mut self) -> Result<Expr<'a>, Error<'a>> {
        Ok(Expr::RefID(self.ref_id()?))
    }

    /// Parse ref identifier (i.e. variable refrence (not &))
    fn ref_id(&mut self) -> Result<ast::RefID<'a>, Error<'a>> {
        let value = self.namespace()?;
        Ok(ast::RefID {
            pos: value.pos,
            type_val: None,
            value: Rc::new(value),
        })
    }

    fn tuple_expr(&mut self) -> Result<Expr<'a>, Error<'a>> {
        Ok(Expr::Tuple(self.tuple()?))
    }

    fn tuple(&mut self) -> Result<ast::Tuple<'a>, Error<'a>> {
        let position = self.token_pos;

        self.next(lexer::TokenType::LP, position, false)?;

        let values = match self.items() {
            Ok(items) => {
                if items.len() == 1 {
                    // Required trailing comma
                    self.next(lexer::TokenType::COMMA, position, false)?;
                } else {
                    // Optional trailing comma
                    if let lexer::TokenType::COMMA = self.peek().token {
                        self.forward();
                    }
                }
                items
            }
            Err(_) => {
                // Optional trailing comma
                if let lexer::TokenType::COMMA = self.peek().token {
                    self.forward();
                };
                Vec::new()
            }
        };
        self.next(lexer::TokenType::RP, position, false)?;

        Ok(ast::Tuple {
            values,
            type_val: None,
            pos: self.get_relative_pos(position),
        })
    }

    fn items(&mut self) -> Result<Vec<Expr<'a>>, Error<'a>> {
        let mut items: Vec<Expr<'_>> = Vec::new();

        let expr = self.expr(Prec::LOWEST)?;
        items.push(expr);

        loop {
            let position = self.token_pos;
            if let lexer::TokenType::COMMA = self.peek().token {
                self.forward();
                if let Ok(expr) = self.expr(Prec::LOWEST) {
                    items.push(expr);
                } else {
                    self.set_pos(position);
                    break;
                }
            } else {
                break;
            }
        }

        Ok(items)
    }

    /// Parse type expression
    fn type_expr(&mut self) -> Result<ast::Type<'a>, Error<'a>> {
        let position = self.token_pos;
        let mut errors: Vec<Error<'_>> = Vec::new();

        match self.namespace_type() {
            Ok(namespace) => return Ok(namespace),
            Err(e) => errors.push(e),
        }

        match self.tuple_type() {
            Ok(tuple) => return Ok(tuple),
            Err(e) => errors.push(e),
        }
        self.set_pos(position);
        Err(Logger::longest(errors))
    }

    fn namespace_type(&mut self) -> Result<ast::Type<'a>, Error<'a>> {
        let namespace = self.namespace()?;

        Ok(ast::Type {
            pos: namespace.pos,
            inferred: false,
            value: ast::TypeType::Type(Rc::new(namespace)),
        })
    }

    fn tuple_type(&mut self) -> Result<ast::Type<'a>, Error<'a>> {
        let position = self.token_pos;

        self.next(lexer::TokenType::LP, position, false)?;

        let values = match self.items_type() {
            Ok(items) => {
                if items.len() == 1 {
                    // Required trailing comma
                    self.next(lexer::TokenType::COMMA, position, false)?;
                } else {
                    // Optional trailing comma
                    if let lexer::TokenType::COMMA = self.peek().token {
                        self.forward();
                    }
                }
                items
            }
            Err(_) => {
                // Optional trailing comma
                if let lexer::TokenType::COMMA = self.peek().token {
                    self.forward();
                };
                Vec::new()
            }
        };
        self.next(lexer::TokenType::RP, position, false)?;

        Ok(ast::Type {
            value: ast::TypeType::Tuple(values),
            inferred: false,
            pos: self.get_relative_pos(position),
        })
    }

    fn items_type(&mut self) -> Result<Vec<Rc<ast::Type<'a>>>, Error<'a>> {
        let mut items: Vec<Rc<ast::Type<'_>>> = Vec::new();

        let type_type = self.type_expr()?;
        items.push(Rc::new(type_type));

        loop {
            let position = self.token_pos;
            if let lexer::TokenType::COMMA = self.peek().token {
                self.forward();
                if let Ok(type_type) = self.type_expr() {
                    items.push(Rc::new(type_type));
                } else {
                    self.set_pos(position);
                    break;
                }
            } else {
                break;
            }
        }

        Ok(items)
    }

    fn dollar_expr(&mut self) -> Result<Expr<'a>, Error<'a>> {
        Ok(Expr::DollarID(self.dollar_id()?))
    }

    /// Parse dollar id
    fn dollar_id(&mut self) -> Result<ast::DollarID<'a>, Error<'a>> {
        let position = self.token_pos;

        self.next(lexer::TokenType::DOLLAR, position, false)?;
        let id = self.namespace()?;

        Ok(ast::DollarID {
            value: Rc::new(id),
            type_val: None,
            pos: self.get_relative_pos(position),
        })
    }
}

#[cfg(test)]
pub mod parser_tests {
    use super::*;
    use crate::helpers::Pos;
    use crate::parser::ast::*;
    use crate::typecheck::ast_typecheck::TypeCheckTypeType;
    const FILENAME: &str = "a_really_long_parser_filename_for_this_test.fl";
    macro_rules! parser_test {
        ($code: expr, $function: expr, $expected: expr, $name: ident) => {
            #[test]
            fn $name() -> Result<(), Error<'static>> {
                let logger = Rc::new(RefCell::new(Logger::new(true)));
                let mut parser = Parser::new(path::Path::new(FILENAME), $code, logger);
                parser.initialize_expr();
                parser.fill_token_stream()?;
                assert_eq!($expected, $function(&mut parser)?);
                Ok(())
            }
        };
    }

    parser_test!(
        "$heloa1234_123",
        Parser::dollar_id,
        DollarID {
            value: Rc::new(Namespace {
                scopes: vec![NameID {
                    value: "heloa1234_123",
                    pos: Pos {
                        s: 2,
                        e: 14,
                        filename: path::Path::new(FILENAME)
                    }
                }],
                pos: Pos {
                    s: 2,
                    e: 14,
                    filename: path::Path::new(FILENAME)
                }
            }),
            type_val: None,
            pos: Pos {
                s: 0,
                e: 14,
                filename: path::Path::new(FILENAME)
            }
        },
        dollar_id
    );

    parser_test!(
        "$heloa1234_123",
        Parser::dollar_expr,
        Expr::DollarID(DollarID {
            value: Rc::new(Namespace {
                scopes: vec![NameID {
                    value: "heloa1234_123",
                    pos: Pos {
                        s: 2,
                        e: 14,
                        filename: path::Path::new(FILENAME)
                    }
                }],
                pos: Pos {
                    s: 2,
                    e: 14,
                    filename: path::Path::new(FILENAME)
                }
            }),
            type_val: None,
            pos: Pos {
                s: 0,
                e: 14,
                filename: path::Path::new(FILENAME)
            }
        }),
        dollar_expr
    );

    parser_test!(
        "(int, int, str)",
        Parser::tuple_type,
        Type {
            value: TypeType::Tuple(vec![
                Rc::new(Type {
                    value: TypeType::Type(Rc::new(Namespace {
                        scopes: vec![NameID {
                            value: "int",
                            pos: Pos {
                                s: 1,
                                e: 4,
                                filename: path::Path::new(FILENAME)
                            }
                        }],
                        pos: Pos {
                            s: 1,
                            e: 4,
                            filename: path::Path::new(FILENAME)
                        }
                    })),
                    inferred: false,
                    pos: Pos {
                        s: 1,
                        e: 4,
                        filename: path::Path::new(FILENAME)
                    }
                }),
                Rc::new(Type {
                    value: TypeType::Type(Rc::new(Namespace {
                        scopes: vec![NameID {
                            value: "int",
                            pos: Pos {
                                s: 6,
                                e: 9,
                                filename: path::Path::new(FILENAME)
                            }
                        }],
                        pos: Pos {
                            s: 6,
                            e: 9,
                            filename: path::Path::new(FILENAME)
                        }
                    })),
                    inferred: false,
                    pos: Pos {
                        s: 6,
                        e: 9,
                        filename: path::Path::new(FILENAME)
                    }
                }),
                Rc::new(Type {
                    value: TypeType::Type(Rc::new(Namespace {
                        scopes: vec![NameID {
                            value: "str",
                            pos: Pos {
                                s: 11,
                                e: 14,
                                filename: path::Path::new(FILENAME)
                            }
                        }],
                        pos: Pos {
                            s: 11,
                            e: 14,
                            filename: path::Path::new(FILENAME)
                        }
                    })),
                    inferred: false,
                    pos: Pos {
                        s: 11,
                        e: 14,
                        filename: path::Path::new(FILENAME)
                    }
                })
            ]),
            inferred: false,
            pos: Pos {
                s: 0,
                e: 15,
                filename: path::Path::new(FILENAME)
            }
        },
        tuple_type_4
    );

    parser_test!(
        "(int,)",
        Parser::tuple_type,
        Type {
            value: TypeType::Tuple(vec![Rc::new(Type {
                value: TypeType::Type(Rc::new(Namespace {
                    scopes: vec![NameID {
                        value: "int",
                        pos: Pos {
                            s: 1,
                            e: 4,
                            filename: path::Path::new(FILENAME)
                        }
                    }],
                    pos: Pos {
                        s: 1,
                        e: 4,
                        filename: path::Path::new(FILENAME)
                    }
                })),
                inferred: false,
                pos: Pos {
                    s: 1,
                    e: 4,
                    filename: path::Path::new(FILENAME)
                }
            }),]),
            inferred: false,
            pos: Pos {
                s: 0,
                e: 6,
                filename: path::Path::new(FILENAME)
            }
        },
        tuple_type_3
    );

    parser_test!(
        "()",
        Parser::tuple_type,
        Type {
            value: TypeType::Tuple(Vec::new()),
            inferred: false,
            pos: Pos {
                s: 0,
                e: 2,
                filename: path::Path::new(FILENAME)
            }
        },
        tuple_type_1
    );

    parser_test!(
        "(,)",
        Parser::tuple_type,
        Type {
            value: TypeType::Tuple(Vec::new()),
            inferred: false,
            pos: Pos {
                s: 0,
                e: 3,
                filename: path::Path::new(FILENAME)
            }
        },
        tuple_type_2
    );

    parser_test!(
        "awd::a12_12a::aw::qwertyuiop1",
        Parser::namespace_type,
        Type {
            value: TypeType::Type(Rc::new(Namespace {
                scopes: vec![
                    NameID {
                        value: "awd",
                        pos: Pos {
                            s: 0,
                            e: 3,
                            filename: path::Path::new(FILENAME)
                        }
                    },
                    NameID {
                        value: "a12_12a",
                        pos: Pos {
                            s: 5,
                            e: 12,
                            filename: path::Path::new(FILENAME)
                        }
                    },
                    NameID {
                        value: "aw",
                        pos: Pos {
                            s: 14,
                            e: 16,
                            filename: path::Path::new(FILENAME)
                        }
                    },
                    NameID {
                        value: "qwertyuiop1",
                        pos: Pos {
                            s: 19,
                            e: 29,
                            filename: path::Path::new(FILENAME)
                        }
                    }
                ],
                pos: Pos {
                    s: 0,
                    e: 29,
                    filename: path::Path::new(FILENAME)
                }
            })),
            inferred: false,
            pos: Pos {
                s: 0,
                e: 29,
                filename: path::Path::new(FILENAME)
            }
        },
        namespace_type_1
    );

    parser_test!(
        "awd::a12_12a",
        Parser::namespace_type,
        Type {
            value: TypeType::Type(Rc::new(Namespace {
                scopes: vec![
                    NameID {
                        value: "awd",
                        pos: Pos {
                            s: 0,
                            e: 3,
                            filename: path::Path::new(FILENAME)
                        }
                    },
                    NameID {
                        value: "a12_12a",
                        pos: Pos {
                            s: 6,
                            e: 12,
                            filename: path::Path::new(FILENAME)
                        }
                    }
                ],
                pos: Pos {
                    s: 0,
                    e: 12,
                    filename: path::Path::new(FILENAME)
                }
            })),
            inferred: false,
            pos: Pos {
                s: 0,
                e: 12,
                filename: path::Path::new(FILENAME)
            }
        },
        namespace_type_2
    );

    parser_test!(
        "()",
        Parser::tuple,
        Tuple {
            values: Vec::new(),
            type_val: None,
            pos: Pos {
                s: 0,
                e: 2,
                filename: path::Path::new(FILENAME)
            }
        },
        tuple_1
    );

    parser_test!(
        "(,)",
        Parser::tuple,
        Tuple {
            values: Vec::new(),
            type_val: None,
            pos: Pos {
                s: 0,
                e: 3,
                filename: path::Path::new(FILENAME)
            }
        },
        tuple_2
    );

    parser_test!(
        "(120, 123, \"ad\")",
        Parser::tuple,
        Tuple {
            values: vec![
                Expr::Literal(Literal {
                    value: "120",
                    type_val: TypeCheckOrType::TypeCheckType(TypeCheckType {
                        value: TypeCheckTypeType::SingleType(Rc::new(Namespace {
                            scopes: vec![NameID {
                                value: "{number}",
                                pos: Pos {
                                    s: 1,
                                    e: 4,
                                    filename: path::Path::new(FILENAME)
                                }
                            }],
                            pos: Pos {
                                s: 1,
                                e: 4,
                                filename: path::Path::new(FILENAME)
                            }
                        })),
                        pos: Pos {
                            s: 1,
                            e: 4,
                            filename: path::Path::new(FILENAME)
                        },
                        inferred: false
                    }),
                    pos: Pos {
                        s: 1,
                        e: 4,
                        filename: path::Path::new(FILENAME)
                    }
                }),
                Expr::Literal(Literal {
                    value: "123",
                    type_val: TypeCheckOrType::TypeCheckType(TypeCheckType {
                        value: TypeCheckTypeType::SingleType(Rc::new(Namespace {
                            scopes: vec![NameID {
                                value: "{number}",
                                pos: Pos {
                                    s: 6,
                                    e: 9,
                                    filename: path::Path::new(FILENAME)
                                }
                            }],
                            pos: Pos {
                                s: 6,
                                e: 9,
                                filename: path::Path::new(FILENAME)
                            }
                        })),
                        pos: Pos {
                            s: 6,
                            e: 9,
                            filename: path::Path::new(FILENAME)
                        },
                        inferred: false
                    }),
                    pos: Pos {
                        s: 6,
                        e: 9,
                        filename: path::Path::new(FILENAME)
                    }
                }),
                Expr::Literal(Literal {
                    value: "\"ad\"",
                    type_val: TypeCheckOrType::TypeCheckType(TypeCheckType {
                        value: TypeCheckTypeType::SingleType(Rc::new(Namespace {
                            scopes: vec![NameID {
                                value: "str",
                                pos: Pos {
                                    s: 11,
                                    e: 15,
                                    filename: path::Path::new(FILENAME)
                                }
                            }],
                            pos: Pos {
                                s: 11,
                                e: 15,
                                filename: path::Path::new(FILENAME)
                            }
                        })),
                        pos: Pos {
                            s: 11,
                            e: 15,
                            filename: path::Path::new(FILENAME)
                        },
                        inferred: false
                    }),
                    pos: Pos {
                        s: 11,
                        e: 15,
                        filename: path::Path::new(FILENAME)
                    }
                })
            ],
            type_val: None,
            pos: Pos {
                s: 0,
                e: 16,
                filename: path::Path::new(FILENAME)
            }
        },
        tuple_3
    );

    parser_test!(
        "test12_1249",
        Parser::ref_id,
        RefID {
            value: Rc::new(Namespace {
                scopes: vec![NameID {
                    value: "test12_1249",
                    pos: Pos {
                        s: 0,
                        e: 11,
                        filename: path::Path::new(FILENAME)
                    }
                }],
                pos: Pos {
                    s: 0,
                    e: 11,
                    filename: path::Path::new(FILENAME)
                }
            }),
            type_val: None,
            pos: Pos {
                s: 0,
                e: 11,
                filename: path::Path::new(FILENAME)
            }
        },
        ref_id
    );

    parser_test!(
        "a123ajd_test_test",
        Parser::ref_expr,
        Expr::RefID(RefID {
            value: Rc::new(Namespace {
                scopes: vec![NameID {
                    value: "a123ajd_test_test",
                    pos: Pos {
                        s: 0,
                        e: 17,
                        filename: path::Path::new(FILENAME)
                    }
                }],
                pos: Pos {
                    s: 0,
                    e: 17,
                    filename: path::Path::new(FILENAME)
                }
            }),
            type_val: None,
            pos: Pos {
                s: 0,
                e: 17,
                filename: path::Path::new(FILENAME)
            }
        }),
        ref_expr
    );

    parser_test!(
        "a123ajd_test_test_qwertyuiopasdfghjklzxcvbnm",
        Parser::name_id,
        NameID {
            value: "a123ajd_test_test_qwertyuiopasdfghjklzxcvbnm",
            pos: Pos {
                s: 0,
                e: 44,
                filename: path::Path::new(FILENAME)
            }
        },
        name_id_1
    );

    parser_test!(
        "a",
        Parser::name_id,
        NameID {
            value: "a",
            pos: Pos {
                s: 0,
                e: 1,
                filename: path::Path::new(FILENAME)
            }
        },
        name_id_2
    );

    parser_test!(
        "\"a123ajd_test_test\"",
        Parser::string_literal,
        Expr::Literal(Literal {
            value: "\"a123ajd_test_test\"",
            type_val: TypeCheckOrType::TypeCheckType(TypeCheckType {
                value: TypeCheckTypeType::SingleType(Rc::new(Namespace {
                    scopes: vec![NameID {
                        value: "str",
                        pos: Pos {
                            s: 0,
                            e: 19,
                            filename: path::Path::new(FILENAME)
                        }
                    }],
                    pos: Pos {
                        s: 0,
                        e: 19,
                        filename: path::Path::new(FILENAME)
                    }
                })),
                pos: Pos {
                    s: 0,
                    e: 19,
                    filename: path::Path::new(FILENAME)
                },
                inferred: false
            }),
            pos: Pos {
                s: 0,
                e: 19,
                filename: path::Path::new(FILENAME)
            }
        }),
        string_literal_1
    );

    parser_test!(
        "\"a\"",
        Parser::string_literal,
        Expr::Literal(Literal {
            value: "\"a\"",
            type_val: TypeCheckOrType::TypeCheckType(TypeCheckType {
                value: TypeCheckTypeType::SingleType(Rc::new(Namespace {
                    scopes: vec![NameID {
                        value: "str",
                        pos: Pos {
                            s: 0,
                            e: 3,
                            filename: path::Path::new(FILENAME)
                        }
                    }],
                    pos: Pos {
                        s: 0,
                        e: 3,
                        filename: path::Path::new(FILENAME)
                    }
                })),
                pos: Pos {
                    s: 0,
                    e: 3,
                    filename: path::Path::new(FILENAME)
                },
                inferred: false
            }),
            pos: Pos {
                s: 0,
                e: 3,
                filename: path::Path::new(FILENAME)
            }
        }),
        string_literal_2
    );

    //#[test]
    fn print_vals() {
        // Utility function for printing ast's
        let logger = Rc::new(RefCell::new(Logger::new(true)));
        let mut parser = Parser::new(path::Path::new(FILENAME), "\"a\"", logger);
        parser.initialize_expr();
        parser
            .fill_token_stream()
            .expect("Failed to fill token stream");
        let output = format!("{:?}", parser.string_literal().expect("failed to parse"))
            .replace("[", "vec![")
            .replace(
                "\"a_really_long_parser_filename_for_this_test.fl\"",
                "path::Path::new(FILENAME)",
            )
            .replace("SingleType(", "TypeCheckTypeType::SingleType(")
            .replace("Literal(", "Expr::Literal(")
            .replace("TypeCheckType(", "TypeCheckOrType::TypeCheckType(");
        println!("{}", output);
        panic!("awd");
    }
}
