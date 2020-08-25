use super::ast;
use super::ast::{Expr, LiteralType, Scope, Statement};

use crate::helpers;
use crate::lexer;
use crate::logger::{ErrorValue, ErrorAnnotation, ErrorDisplayType, ErrorType, Logger, LoggerInner};
use crate::paths;
use crate::sourcemap::SourceMap;
use crate::tags::UnitTags;

use std::collections::HashMap;
use std::path;
use std::rc::Rc;

macro_rules! run_all {
    ( $self: expr, $($e:expr),+ ) => {
        $(ignore_or_return!($e($self));)+
    };
}

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
pub struct Parser {
    /// Lexer object
    pub lexer: lexer::Lexer,
    /// Abstract syntax tree
    pub ast: Option<ast::Block>,
    logger: Logger,

    statements: [fn(&mut Self) -> Result<Statement, ErrorValue>; 12],
    prefix_op: HashMap<lexer::TokenType, Prec>,
    infix_op: HashMap<lexer::TokenType, Prec>,
    tokens: Vec<lexer::Token>,
    token_pos: usize,
    sourcemap: SourceMap,
}

impl Parser {
    /// Return a new parser object.
    pub fn new(filename: usize, logger: Logger, sourcemap: SourceMap) -> Parser {
        let l = lexer::Lexer::new(filename, Rc::clone(&sourcemap));
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
            sourcemap,
        }
    }

    pub fn load_from_path<'a>(&mut self, source_path: &'a path::Path) {
        let mut contents = paths::read_file(&source_path);
        let file_id = self.sourcemap.borrow_mut().insert_file(source_path.to_path_buf(), contents);

        let mut parser = Parser::new(file_id, Rc::clone(&self.logger), Rc::clone(&self.sourcemap));
        parser.initialize_expr();
        helpers::error_or_other(parser.parse(), Rc::clone(&self.logger));

        self.ast.as_mut().unwrap().nodes.append(&mut parser.ast.unwrap().nodes);
    }

    /// Template for syntax error
    pub fn syntax_error(
        &self,
        t: lexer::Token,
        message: &str,
        is_keyword: bool,
        urgent: bool,
    ) -> ErrorValue {
        ErrorValue::new(
            if !is_keyword {
                format!("{}, found {}", message, t.f(Rc::clone(&self.sourcemap)))
            } else {
                format!("unexpected {}", t.f(Rc::clone(&self.sourcemap)))
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
        token_type: lexer::TokenType,
        position: usize,
        is_keyword: bool,
    ) -> Result<(), ErrorValue> {
        let prev_pos = self.token_pos;
        let t = self.forward();

        if t.token != token_type {
            let error = format!("expected {}", token_type.f(Rc::clone(&self.sourcemap)));
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

    pub fn get_relative_pos(&mut self, position: usize) -> helpers::Pos {
        helpers::Pos {
            s: self.tokens[position].pos.s,
            e: self.tokens[if self.token_pos > 0 {
                self.token_pos - 1
            } else {
                self.token_pos
            }]
            .pos
            .e,
            filename_id: self.lexer.filename,
        }
    }

    pub fn initialize_expr(&mut self) {
        // `-`
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

    pub fn register_prefix(&mut self, token: lexer::TokenType, prec: Prec) {
        self.prefix_op.insert(token, prec);
    }

    pub fn register_infix(&mut self, token: lexer::TokenType, prec: Prec) {
        self.infix_op.insert(token, prec);
    }

    fn fill_token_stream(&mut self) -> Result<(), ErrorValue> {
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

    fn peek(&self) -> lexer::Token {
        self.tokens[self.token_pos]
    }

    fn forward(&mut self) -> lexer::Token {
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
    pub fn parse(&mut self) -> Result<(), Vec<ErrorValue>> {
        if let Err(e) = self.fill_token_stream() {
            return Err(vec![e]);
        }

        // Set our scope to outside
        let scope = Scope::Outer;
        let position = self.token_pos;

        let mut ast_list: Vec<Statement> = Vec::new();
        let next = self.peek();
        if next.token != lexer::TokenType::EOF {
            loop {
                let mut errors: Vec<ErrorValue> = Vec::new();
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
                                errors.push(ErrorValue::new(
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
                    return Err(vec![LoggerInner::longest(errors)]);
                }
            }
        }

        let block = ast::Block {
            nodes: ast_list,
            tags: UnitTags::new(),
            pos: self.get_relative_pos(position),
            insert_return: false,
            returns: false,
        };
        self.ast = Some(block);
        Ok(())
    }

    /// Parse basic block
    pub fn block(&mut self, scope: Scope) -> Result<ast::Block, ErrorValue> {
        let position = self.token_pos;
        self.next(lexer::TokenType::LCP, position, false)?;

        let mut ast_list: Vec<Statement> = Vec::new();
        loop {
            let mut errors: Vec<ErrorValue> = Vec::new();
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
                            errors.push(ErrorValue::new(
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
                return Err(LoggerInner::longest(errors));
            } else if self.peek().token != lexer::TokenType::RCP && fail {
                // We've forgotten closing brace
                break;
            }
        }

        // Check for closing brace here
        self.next(lexer::TokenType::RCP, position, false)?;

        Ok(ast::Block {
            nodes: ast_list,
            tags: UnitTags::new(),
            pos: self.get_relative_pos(position),
            insert_return: false,
            returns: false,
        })
    }

    fn parse_arguments(&mut self) -> Result<ast::Arguments, ErrorValue> {
        let position = self.token_pos;
        let mut positional_args: Vec<(ast::NameID, ast::Type)> = Vec::new();

        loop {
            if self.peek().token == lexer::TokenType::RP {
                // No error, we've reached the end
                break;
            }

            let id = self.name_id()?;

            self.next(lexer::TokenType::COLON, position, false)?;

            let arg_type = self.type_expr()?;

            positional_args.push((id, arg_type));
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

    fn type_assign(&mut self) -> Result<Statement, ErrorValue> {
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
            value,
            name: Rc::new(name),
            visibility,
            pos: self.get_relative_pos(position),
        }))
    }

    fn unit(&mut self) -> Result<Statement, ErrorValue> {
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

    fn add_file(&mut self, name: &mut ast::Namespace) -> Result<Statement, ErrorValue> {
        let mut scopes = Vec::new();
        std::mem::swap(&mut scopes, &mut name.scopes);

        let mut import_path = path::PathBuf::new();
        import_path.push(get_filename!(self.sourcemap, self.lexer.filename));
        import_path = paths::get_parent(paths::canonicalize_file(&import_path));

        let mut last_idx: usize = 0;
        for (idx, op) in scopes.iter().enumerate() {
            if import_path
                .join(format!("{}.fl", get_segment!(self.sourcemap, op.pos)))
                .is_file()
            {
                import_path.push(format!("{}.fl", get_segment!(self.sourcemap, op.pos)));
                // We've reached a file, break
                last_idx = idx + 1;
                break;
            } else if import_path.join(get_segment!(self.sourcemap, op.pos)).is_dir() {
                // We've reached a directory
                import_path.push(get_segment!(self.sourcemap, op.pos));
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
            Err(ErrorValue::new(
                "file does not exist".to_string(),
                ErrorType::ImportError,
                last.pos,
                ErrorDisplayType::Error,
                vec![ErrorAnnotation::new(
                    Some(format!("file `{}` does not exist", {
                        import_path.push(format!("{}.fl", get_segment!(self.sourcemap, last.pos)));
                        import_path.display()
                    })),
                    last.pos,
                    ErrorDisplayType::Error,
                )],
                true,
            ))
        } else if scopes.is_empty() {
            let file_contents = paths::read_file(&import_path);
            let file_id = self
                .sourcemap
                .borrow_mut()
                .insert_file(import_path, file_contents);
            let mut parser =
                Parser::new(file_id, Rc::clone(&self.logger), Rc::clone(&self.sourcemap));
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
            Err(ErrorValue::new(
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
    fn import(&mut self) -> Result<Statement, ErrorValue> {
        let position = self.token_pos;
        self.next(lexer::TokenType::UNIT, position, true)?;

        let mut namespace = self.namespace()?;

        self.next(lexer::TokenType::SEMI, position, false)?;

        self.add_file(&mut namespace)
    }

    fn compiler_tag(&mut self) -> Result<Statement, ErrorValue> {
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
    fn function_define(&mut self) -> Result<Statement, ErrorValue> {
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
        let return_type: ast::Type = if self.peek().token == lexer::TokenType::ARROW {
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
            return_type,
            arguments,
            block: Some(block),
            visibility,
            name,
            mangled_name: None,
            pos: self.get_relative_pos(position),
            overload_operator: None,
        }))
    }

    fn extern_def(&mut self) -> Result<Statement, ErrorValue> {
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

        let return_type: ast::Type = if self.peek().token == lexer::TokenType::ARROW {
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
            return_type,
            arguments,
            visibility,
            name,
            mangled_name: None,
            pos: self.get_relative_pos(position),
            block: None, // No block on external function
            overload_operator: None,
        }))
    }

    fn overload_define(&mut self) -> Result<Statement, ErrorValue> {
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
        let return_type: ast::Type = if self.peek().token == lexer::TokenType::ARROW {
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
            return_type,
            arguments,
            block: Some(block),
            visibility,
            name,
            mangled_name: None,
            pos: self.get_relative_pos(position),
            overload_operator,
        }))
    }

    fn overload_extern(&mut self) -> Result<Statement, ErrorValue> {
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

        let return_type: ast::Type = if self.peek().token == lexer::TokenType::ARROW {
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
            return_type,
            arguments,
            visibility,
            name,
            mangled_name: None,
            pos: self.get_relative_pos(position),
            block: None, // No block on external function
            overload_operator,
        }))
    }

    fn parse_extern_arguments(&mut self) -> Result<ast::Arguments, ErrorValue> {
        let position = self.token_pos;
        let mut positional_args: Vec<(ast::NameID, ast::Type)> = Vec::new();

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
                positional_args.push((id_val.clone(), arg_type));
            } else {
                // No name id, its optional so this is fine
                self.token_pos = position;
                let arg_type = self.type_expr()?;
                positional_args.push((
                    ast::NameID {
                        sourcemap: Rc::clone(&self.sourcemap),
                        pos: self.get_relative_pos(position),
                    },
                    arg_type,
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
    fn return_statement(&mut self) -> Result<Statement, ErrorValue> {
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
    fn expression_statement(&mut self) -> Result<Statement, ErrorValue> {
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
    fn arguments_call(&mut self) -> Result<ast::ArgumentsRun, ErrorValue> {
        let position = self.token_pos;
        let mut positional_args: Vec<Expr> = Vec::new();

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
    fn function_call(&mut self) -> Result<Expr, ErrorValue> {
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
    fn variable_assign_full(&mut self) -> Result<Expr, ErrorValue> {
        let position = self.token_pos;
        self.next(lexer::TokenType::LET, position, true)?;

        let namespace = self.namespace()?;

        self.next(lexer::TokenType::COLON, position, false)?;
        let var_type = self.type_expr()?;

        self.next(lexer::TokenType::EQUALS, position, false)?;
        let expr = self.expr(Prec::LOWEST)?;

        Ok(ast::Expr::VariableAssignDeclaration(
            ast::VariableAssignDeclaration {
                t: var_type,
                name: Rc::new(namespace),
                expr: Box::new(expr),
                pos: self.get_relative_pos(position),
            },
        ))
    }

    /// Variable Declaration
    fn variable_declaration(&mut self) -> Result<Statement, ErrorValue> {
        let position = self.token_pos;
        self.next(lexer::TokenType::LET, position, true)?;

        let namespace = self.namespace()?;

        self.next(lexer::TokenType::COLON, position, false)?;
        let var_type = self.type_expr()?;

        self.next(lexer::TokenType::SEMI, position, false)?;

        Ok(ast::Statement::VariableDeclaration(
            ast::VariableDeclaration {
                t: var_type,
                name: Rc::new(namespace),
                pos: self.get_relative_pos(position),
            },
        ))
    }

    /// Variable assign with only expression
    fn variable_assign(&mut self) -> Result<Expr, ErrorValue> {
        let position = self.token_pos;

        let namespace = self.namespace()?;

        self.next(lexer::TokenType::EQUALS, position, false)?;

        let expr = self.expr(Prec::LOWEST)?;

        Ok(ast::Expr::VariableAssign(ast::VariableAssign {
            name: Rc::new(namespace),
            expr: Box::new(expr),
            pos: self.get_relative_pos(position),
        }))
    }

    fn conditional(&mut self) -> Result<Statement, ErrorValue> {
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

    fn if_cond(&mut self) -> Result<ast::IfBranch, ErrorValue> {
        let position = self.token_pos;

        self.next(lexer::TokenType::IF, position, true)?;
        let cond = self.expr(Prec::LOWEST)?;

        let block = self.block(Scope::Block)?;

        Ok(ast::IfBranch {
            cond,
            block,
            pos: self.get_relative_pos(position),
        })
    }

    fn else_if_cond(&mut self) -> Result<ast::IfBranch, ErrorValue> {
        let position = self.token_pos;

        self.next(lexer::TokenType::ELSE, position, true)?;
        self.next(lexer::TokenType::IF, position, true)?;

        let cond = self.expr(Prec::LOWEST)?;

        let block = self.block(Scope::Block)?;

        Ok(ast::IfBranch {
            cond,
            block,
            pos: self.get_relative_pos(position),
        })
    }

    fn else_cond(&mut self) -> Result<ast::ElseBranch, ErrorValue> {
        let position = self.token_pos;

        self.next(lexer::TokenType::ELSE, position, true)?;
        let block = self.block(Scope::Block)?;

        Ok(ast::ElseBranch {
            block,
            pos: self.get_relative_pos(position),
        })
    }

    /// Top level expression
    fn expr(&mut self, prec: Prec) -> Result<Expr, ErrorValue> {
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

    fn get_operator_infix(&mut self) -> Result<lexer::Token, ErrorValue> {
        let position = self.token_pos;
        let potential_op = self.forward();
        for op in self.infix_op.keys() {
            if &potential_op.token == op {
                return Ok(potential_op);
            }
        }
        let temp = Err(ErrorValue::new(
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

    fn get_operator_prefix(&mut self) -> Result<lexer::Token, ErrorValue> {
        let position = self.token_pos;
        let potential_op = self.forward();
        for op in self.prefix_op.keys() {
            if &potential_op.token == op {
                return Ok(potential_op);
            }
        }

        let temp = Err(ErrorValue::new(
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

    fn binding_power(&mut self, token_type: &lexer::TokenType) -> Prec {
        self.infix_op[token_type]
    }

    fn led(&mut self, left: Expr, operator: lexer::Token) -> Result<Expr, ErrorValue> {
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

    fn item(&mut self) -> Result<Expr, ErrorValue> {
        let position = self.token_pos;

        if let Ok(prefix) = self.get_operator_prefix() {
            let bp = self.binding_power(&prefix.token);
            let item = self.expr(bp)?;
            return Ok(Expr::Prefix(ast::Prefix {
                operator: prefix,
                val: Box::new(item),
                pos: self.get_relative_pos(position),
            }));
        }

        run_all! {
            self,
            Parser::integer,
            Parser::string_literal,
            Parser::bool_expr,
            Parser::function_call,
            Parser::variable_assign_full,
            Parser::variable_assign,
            Parser::tuple_expr,
            Parser::dollar_expr,
            Parser::ref_expr
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
        let temp = Err(ErrorValue::new(
            "Missing expression".to_string(),
            ErrorType::Syntax,
            self.get_relative_pos(position),
            ErrorDisplayType::Error,
            vec![ErrorAnnotation::new(
                Some(format!(
                    "Expected expression, found {}",
                    next.f(Rc::clone(&self.sourcemap))
                )),
                self.get_relative_pos(position),
                ErrorDisplayType::Error,
            )],
            false,
        ));
        self.set_pos(position);
        temp
    }

    fn bool_expr(&mut self) -> Result<Expr, ErrorValue> {
        let position = self.token_pos;

        let possible_bool = self.forward();
        if lexer::TokenType::TRUE == possible_bool.token
            || lexer::TokenType::FALSE == possible_bool.token
        {
            Ok(ast::Expr::Literal(ast::Literal {
                literal_type: LiteralType::Bool,
                pos: possible_bool.pos,
            }))
        } else {
            let temp = Err(self.syntax_error(possible_bool, "expected bool", false, false));
            self.set_pos(position);
            temp
        }
    }

    fn integer(&mut self) -> Result<Expr, ErrorValue> {
        let position = self.token_pos;

        let int = self.forward();
        if lexer::TokenType::NUMBER == int.token {
            Ok(ast::Expr::Literal(ast::Literal {
                literal_type: LiteralType::Integer,
                pos: int.pos,
            }))
        } else {
            let temp = Err(self.syntax_error(int, "expected integer", false, false));
            self.set_pos(position);
            temp
        }
    }

    fn string_literal(&mut self) -> Result<Expr, ErrorValue> {
        let position = self.token_pos;

        let string = self.forward();
        if lexer::TokenType::STRING == string.token {
            Ok(ast::Expr::Literal(ast::Literal {
                pos: string.pos,
                literal_type: LiteralType::String,
            }))
        } else {
            let temp = Err(self.syntax_error(string, "expected string literal", false, false));
            self.set_pos(position);
            temp
        }
    }

    fn namespace(&mut self) -> Result<ast::Namespace, ErrorValue> {
        let position = self.token_pos;
        let mut ids: Vec<ast::NameID> = Vec::new();
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
    fn name_id(&mut self) -> Result<ast::NameID, ErrorValue> {
        let position = self.token_pos;
        let id = self.forward();
        if lexer::TokenType::IDENTIFIER == id.token {
            Ok(ast::NameID {
                sourcemap: Rc::clone(&self.sourcemap),
                pos: id.pos,
            })
        } else {
            let temp = Err(self.syntax_error(id, "expected identifier", false, false));
            self.set_pos(position);
            temp
        }
    }

    fn ref_expr(&mut self) -> Result<Expr, ErrorValue> {
        Ok(Expr::RefID(self.ref_id()?))
    }

    /// Parse ref identifier (i.e. variable refrence (not &))
    fn ref_id(&mut self) -> Result<ast::RefID, ErrorValue> {
        let value = self.namespace()?;
        Ok(ast::RefID {
            pos: value.pos,
            value: Rc::new(value),
        })
    }

    fn tuple_expr(&mut self) -> Result<Expr, ErrorValue> {
        Ok(Expr::Tuple(self.tuple()?))
    }

    fn tuple(&mut self) -> Result<ast::Tuple, ErrorValue> {
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
            pos: self.get_relative_pos(position),
        })
    }

    fn items(&mut self) -> Result<Vec<Expr>, ErrorValue> {
        let mut items: Vec<Expr> = Vec::new();

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
    fn type_expr(&mut self) -> Result<ast::Type, ErrorValue> {
        let position = self.token_pos;
        let mut errors: Vec<ErrorValue> = Vec::new();

        match self.namespace_type() {
            Ok(namespace) => return Ok(namespace),
            Err(e) => errors.push(e),
        }

        match self.tuple_type() {
            Ok(tuple) => return Ok(tuple),
            Err(e) => errors.push(e),
        }
        self.set_pos(position);
        Err(LoggerInner::longest(errors))
    }

    fn namespace_type(&mut self) -> Result<ast::Type, ErrorValue> {
        let namespace = self.namespace()?;

        Ok(ast::Type {
            pos: namespace.pos,
            inferred: false,
            value: ast::TypeType::Type(Rc::new(namespace)),
        })
    }

    fn tuple_type(&mut self) -> Result<ast::Type, ErrorValue> {
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

    fn items_type(&mut self) -> Result<Vec<Rc<ast::Type>>, ErrorValue> {
        let mut items: Vec<Rc<ast::Type>> = Vec::new();

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

    fn dollar_expr(&mut self) -> Result<Expr, ErrorValue> {
        Ok(Expr::DollarID(self.dollar_id()?))
    }

    /// Parse dollar id
    fn dollar_id(&mut self) -> Result<ast::DollarID, ErrorValue> {
        let position = self.token_pos;

        self.next(lexer::TokenType::DOLLAR, position, false)?;
        let id = self.namespace()?;

        Ok(ast::DollarID {
            value: Rc::new(id),
            pos: self.get_relative_pos(position),
        })
    }
}

#[cfg(test)]
pub mod parser_tests {
    use super::*;
    use crate::sourcemap::SourceMapInner;
    const FILENAME: &str = "a_really_long_parser_filename_for_this_test.fl";

    macro_rules! parser_run {
        ($code: expr, $function: expr, $name: ident) => {
            #[test]
            fn $name() -> Result<(), ErrorValue> {
                let sourcemap = SourceMapInner::new();
                let filename_code = insert_file!(sourcemap, path::PathBuf::from(FILENAME), $code.to_string());
                let logger = LoggerInner::new(true, Rc::clone(&sourcemap));
                let mut parser = Parser::new(filename_code, logger, sourcemap);
                parser.initialize_expr();
                parser.fill_token_stream()?;
                $function(&mut parser)?;
                Ok(())
            }
        };
    }

    parser_run!("$heloa1234_123", Parser::dollar_id, dollar_id);

    parser_run!("$heloa1234_123", Parser::dollar_expr, dollar_expr);

    parser_run!("(int, int, str)", Parser::tuple_type, tuple_type_4);

    parser_run!("(int,)", Parser::tuple_type, tuple_type_3);

    parser_run!("()", Parser::tuple_type, tuple_type_1);

    parser_run!("(,)", Parser::tuple_type, tuple_type_2);

    parser_run!(
        "awd::a12_12a::aw::qwertyuiop1",
        Parser::namespace_type,
        namespace_type_1
    );

    parser_run!("awd::a12_12a", Parser::namespace_type, namespace_type_2);

    parser_run!("()", Parser::tuple, tuple_1);

    parser_run!("(,)", Parser::tuple, tuple_2);

    parser_run!("(120, 123, \"ad\")", Parser::tuple, tuple_3);

    parser_run!("test12_1249", Parser::ref_id, ref_id);

    parser_run!("a123ajd_test_test", Parser::ref_expr, ref_expr);

    parser_run!(
        "a123ajd_test_test_qwertyuiopasdfghjklzxcvbnm",
        Parser::name_id,
        name_id_1
    );

    parser_run!("a", Parser::name_id, name_id_2);

    parser_run!(
        "\"a123ajd_test_test\"",
        Parser::string_literal,
        string_literal_1
    );

    parser_run!("\"a\"", Parser::string_literal, string_literal_2);

    parser_run!("12930", Parser::integer, integer_literal_1);

    parser_run!("1", Parser::integer, integer_literal_2);

    parser_run!("true", Parser::bool_expr, bool_true);

    parser_run!("false", Parser::bool_expr, bool_false);

    parser_run!("if  true  { } else {}", Parser::conditional, conditional_1);

    parser_run!(
        "if  true  { } else if  true  {} else {}",
        Parser::conditional,
        conditional_2
    );

    parser_run!("if  true  { }", Parser::conditional, conditional_3);

    parser_run!("x = 10", Parser::variable_assign, variable_assign);

    parser_run!(
        "let x: (int, int) = 10",
        Parser::variable_assign_full,
        variable_assign_full
    );

    parser_run!(
        "let x: int;",
        Parser::variable_declaration,
        variable_declaration
    );

    parser_run!(
        "tests::j12::hello(\"hello\", 123,4,2,3,1,true,false,)",
        Parser::function_call,
        function_call
    );

    parser_run!(
        "tests::j12::hello()",
        Parser::function_call,
        function_call_empty
    );

    parser_run!(
        "return ((let x: int = 10, x = 10, hello, 1, \"another_test\"));",
        Parser::return_statement,
        return_statement
    );

    parser_run!(
        "extern overload + add_overload_test9(int, int) -> int;",
        Parser::overload_extern,
        overload_extern
    );

    parser_run!(
        "overload + add_overload_test9(val: int, val: int) -> int {}",
        Parser::overload_define,
        overload_define
    );

    parser_run!(
        "       def add_overload_test9(val: int, val: int) -> int {}",
        Parser::function_define,
        function_define
    );

    parser_run!(
        "extern def        add_overload_test9(int, int) -> int;",
        Parser::extern_def,
        extern_def
    );

    parser_run!("@[no_mangle]", Parser::compiler_tag, compilier_tag);
    parser_run!("(19)", Parser::item, int_1_paren);
    parser_run!("(1)", Parser::item, int_2_paren);
    parser_run!("1", Parser::item, int_1);
    parser_run!("19", Parser::item, int_2);

    parser_run!("(true)", Parser::item, bool_true_paren);
    parser_run!("(false)", Parser::item, bool_false_paren);
    parser_run!("true", Parser::item, bool_true_item);
    parser_run!("false", Parser::item, bool_false_item);

    parser_run!("\"dab\"", Parser::item, string_literal_1_item);
    parser_run!("\"\"", Parser::item, string_literal_2_item);
    parser_run!("(\"dab\")", Parser::item, string_literal_1_paren);
    parser_run!("(\"\")", Parser::item, string_literal_2_paren);

    parser_run!("$hello", Parser::item, dollar_expr_item);
    parser_run!("($hello)", Parser::item, dollar_expr_paren);

    parser_run!("a(10, 10, \"hello\")", Parser::item, call_expr_1_item);
    parser_run!("a(10, 10,)", Parser::item, call_expr_2_item);
    parser_run!("(a(10, 10, \"hello\"))", Parser::item, call_expr_1_paren);
    parser_run!("(a(10, 10,))", Parser::item, call_expr_2_paren);

    parser_run!("(x = 5)", Parser::item, variable_assign_paren);
    parser_run!("x = 5", Parser::item, variable_assign_item);

    parser_run!("(let x: int = 5)", Parser::item, variable_assign_full_paren);
    parser_run!("let x: int = 5", Parser::item, variable_assign_full_item);

    parser_run!("(i)", Parser::item, ref_id_paren);
    parser_run!("i3_hello_world", Parser::item, ref_id_item);

    parser_run!(
        "(let x: int = 10, x = 10, hello, 1, \"another_test\")",
        Parser::item,
        tuple_item
    );
    parser_run!(
        "((let x: int = 10, x = 10, hello, 1, \"another_test\"))",
        Parser::item,
        tuple_paren
    );

    parser_run!("(19);", Parser::item, int_1_paren_stmt);
    parser_run!("(1);", Parser::item, int_2_paren_stmt);
    parser_run!("1;", Parser::item, int_1_stmt);
    parser_run!("19;", Parser::item, int_2_stmt);

    parser_run!("(true);", Parser::item, bool_true_paren_stmt);
    parser_run!("(false);", Parser::item, bool_false_paren_stmt);
    parser_run!("true;", Parser::item, bool_true_item_stmt);
    parser_run!("false;", Parser::item, bool_false_item_stmt);

    parser_run!("\"dab\";", Parser::item, string_literal_1_item_stmt);
    parser_run!("\"\";", Parser::item, string_literal_2_item_stmt);
    parser_run!("(\"dab\");", Parser::item, string_literal_1_paren_stmt);
    parser_run!("(\"\");", Parser::item, string_literal_2_paren_stmt);

    parser_run!("$hello;", Parser::item, dollar_expr_item_stmt);
    parser_run!("($hello);", Parser::item, dollar_expr_paren_stmt);

    parser_run!("a(10, 10, \"hello\");", Parser::item, call_expr_1_item_stmt);
    parser_run!("a(10, 10,);", Parser::item, call_expr_2_item_stmt);
    parser_run!(
        "(a(10, 10, \"hello\"));",
        Parser::item,
        call_expr_1_paren_stmt
    );
    parser_run!("(a(10, 10,));", Parser::item, call_expr_2_paren_stmt);

    parser_run!("(x = 5);", Parser::item, variable_assign_paren_stmt);
    parser_run!("x = 5;", Parser::item, variable_assign_item_stmt);

    parser_run!(
        "(let x: int = 5);",
        Parser::item,
        variable_assign_full_paren_stmt
    );
    parser_run!(
        "let x: int = 5;",
        Parser::item,
        variable_assign_full_item_stmt
    );

    parser_run!("(i);", Parser::item, ref_id_paren_stmt);
    parser_run!("i3_hello_world;", Parser::item, ref_id_item_stmt);

    parser_run!(
        "(let x: int = 10, x = 10, hello, 1, \"another_test\");",
        Parser::item,
        tuple_item_stmt
    );
    parser_run!(
        "((let x: int = 10, x = 10, hello, 1, \"another_test\"));",
        Parser::item,
        tuple_paren_stmt
    );

    macro_rules! parser_err {
        ($code: expr, $function: expr, $name: ident) => {
            #[test]
            fn $name() -> Result<(), ErrorValue> {
                let sourcemap = SourceMapInner::new();
                let filename_code = insert_file!(sourcemap, path::PathBuf::from(FILENAME), $code.to_string());
                let logger = LoggerInner::new(true, Rc::clone(&sourcemap));
                let mut parser = Parser::new(filename_code, logger, sourcemap);

                parser.initialize_expr();
                parser.fill_token_stream()?;
                assert!($function(&mut parser).is_err());
                Ok(())
            }
        };
    }

    parser_err!("hello::", Parser::namespace, unmatched_namespace_err_1);

    parser_err!("::", Parser::namespace, unmatched_namespace_err_2);

    parser_err!("hello::123", Parser::namespace, bad_nameid_namespace_err_1);

    parser_err!(
        "hello 1,2,3,4)",
        Parser::function_call,
        unmatched_opening_paren_function_call_1
    );

    parser_err!(
        "hello(1,2,3,4",
        Parser::function_call,
        unmatched_opening_paren_function_call_2
    );

    parser_err!(
        "hello(1,2,3,4,,)",
        Parser::function_call,
        extra_comma_function_call_1
    );

    parser_err!(
        "hello(1,,2,3,4,)",
        Parser::function_call,
        extra_comma_function_call_2
    );

    parser_err!(
        "hello(1 2,3,4,)",
        Parser::function_call,
        mssing_comma_function_call_1
    );

    parser_err!(
        "hello(1,2 3,4,)",
        Parser::function_call,
        mssing_comma_function_call_2
    );

    //#[test]
    fn print_vals() {
        // Utility function for printing ast's
        let sourcemap = SourceMapInner::new();
        let logger = LoggerInner::new(true, Rc::clone(&sourcemap));
        let mut parser = Parser::new(0, logger, sourcemap);
        parser.initialize_expr();
        parser
            .fill_token_stream()
            .expect("Failed to fill token stream");
        let output = format!("{:?}", parser.import().expect("failed to parse"))
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
