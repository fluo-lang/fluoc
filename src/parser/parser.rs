use super::ast;
use super::ast::{Expr, LiteralType, Scope, Statement};

use crate::helpers;
use crate::lexer;
use crate::logger::{
    ErrorAnnotation, ErrorDisplayType, ErrorGen, ErrorType, ErrorValue, Logger, LoggerInner,
};
use crate::paths;
use crate::sourcemap::SourceMap;
use crate::tags::UnitTags;

use std::collections::HashMap;
use std::path;
use std::rc::Rc;

use smallvec::SmallVec;

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
                if e.urgent {
                    return Err(e);
                }
            }
        }
    };
}

#[derive(Copy, Clone, PartialEq, PartialOrd)]
pub enum Prec {
    Lowest = 0,

    /// Comparison operators
    Comp = 1,

    /// `+` and `-`
    Term = 2,

    /// `*` and `/` and `%`
    Factor = 3,

    /// `as` and `is` operator (CONVersion operator)
    Conv = 4,

    /// `-` (negate) and others (i.e. `!` logical negate)
    Prefix = 5,
}

/// Recursive descent parser
pub struct Parser {
    /// Lexer object
    pub lexer: lexer::Lexer,
    /// Abstract syntax tree
    pub ast: Option<Vec<ast::Statement>>,
    logger: Logger,

    statements: [fn(&mut Self) -> Result<Statement, ErrorGen>; 5],
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
                Parser::expression_statement,
                Parser::type_assign,
                Parser::compiler_tag,
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
        let contents = paths::read_file(&source_path);
        let file_id = self
            .sourcemap
            .borrow_mut()
            .insert_file(source_path.to_path_buf(), contents);

        let mut parser = Parser::new(file_id, Rc::clone(&self.logger), Rc::clone(&self.sourcemap));
        parser.initialize_expr();
        helpers::error_or_other(parser.parse(), Rc::clone(&self.logger));

        self.ast.as_mut().unwrap().append(&mut parser.ast.unwrap());
    }

    /// Template for syntax error
    pub fn syntax_error(
        &self,
        real_token: lexer::Token,
        expected_token: lexer::TokenType,
        is_keyword: bool,
        urgent: bool,
    ) -> ErrorGen {
        let cloned_sourcemap = Rc::clone(&self.sourcemap);
        ErrorGen::new(
            Box::new(move || {
                let real_token_display = real_token.f(Rc::clone(&cloned_sourcemap));
                ErrorValue::new(
                    if !is_keyword {
                        format!(
                            "expected {}, found {}",
                            expected_token.f(),
                            real_token_display
                        )
                    } else {
                        format!("unexpected {}", real_token_display)
                    },
                    ErrorType::Syntax,
                    real_token.pos,
                    ErrorDisplayType::Error,
                    vec![ErrorAnnotation::new(
                        Some("unexpected token".to_string()),
                        real_token.pos,
                        ErrorDisplayType::Error,
                    )],
                )
            }),
            real_token.pos,
            urgent,
        )
    }

    #[inline]
    /// Validate next token
    pub fn validate_token(
        &mut self,
        token_type: lexer::TokenType,
        position: usize,
        is_keyword: bool,
    ) -> Result<helpers::Span, ErrorGen> {
        let t = self.forward();

        if t.token != token_type {
            let temp = Err(self.syntax_error(t, token_type, is_keyword, false));
            self.set_pos(position);
            temp
        } else {
            Ok(t.pos)
        }
    }

    #[inline]
    pub fn get_relative_pos(&mut self, position: usize) -> helpers::Span {
        helpers::Span::new(
            self.tokens[position].pos.s,
            self.tokens[if self.token_pos > 0 {
                self.token_pos - 1
            } else {
                self.token_pos
            }]
            .pos
            .e,
            self.lexer.filename,
        )
    }

    pub fn initialize_expr(&mut self) {
        // `-`
        self.register_prefix(lexer::TokenType::Sub, Prec::Prefix);

        // `-`
        self.register_infix(lexer::TokenType::Sub, Prec::Term);
        // `+`
        self.register_infix(lexer::TokenType::Add, Prec::Term);

        // `/`
        self.register_infix(lexer::TokenType::Div, Prec::Factor);
        // `%`
        self.register_infix(lexer::TokenType::Mod, Prec::Factor);
        // `*`
        self.register_infix(lexer::TokenType::Mul, Prec::Factor);
        // `%%`
        self.register_infix(lexer::TokenType::DMod, Prec::Factor);

        // `as`
        self.register_infix(lexer::TokenType::As, Prec::Conv);
        // `is`
        self.register_infix(lexer::TokenType::Is, Prec::Conv);

        // `>`
        self.register_infix(lexer::TokenType::GT, Prec::Comp);
        // `<`
        self.register_infix(lexer::TokenType::LT, Prec::Comp);
        // `>=`
        self.register_infix(lexer::TokenType::GE, Prec::Comp);
        // `<=`
        self.register_infix(lexer::TokenType::LE, Prec::Comp);
        // `==`
        self.register_infix(lexer::TokenType::EQ, Prec::Comp);
    }

    #[inline]
    pub fn register_prefix(&mut self, token: lexer::TokenType, prec: Prec) {
        self.prefix_op.insert(token, prec);
    }

    #[inline]
    pub fn register_infix(&mut self, token: lexer::TokenType, prec: Prec) {
        self.infix_op.insert(token, prec);
    }

    fn fill_token_stream(&mut self) -> Result<(), ErrorValue> {
        self.tokens.append(&mut self.lexer.get_tokens()?);
        Ok(())
    }

    #[inline]
    fn peek(&self) -> lexer::Token {
        self.tokens[self.token_pos]
    }

    #[inline]
    fn forward(&mut self) -> lexer::Token {
        let temp = self.tokens[self.token_pos];
        self.token_pos += 1;
        temp
    }

    #[inline]
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

        let mut ast_list: Vec<Statement> = Vec::new();
        let next = self.peek();
        if next.token != lexer::TokenType::EOF {
            loop {
                let mut errors: Vec<ErrorGen> = Vec::new();
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
                                let ast_pos = ast_production.pos();
                                errors.push(ErrorGen::new(
                                    Box::new(move || {
                                        ErrorValue::new(
                                            "unexpected statement in outer scope".to_string(),
                                            ErrorType::Syntax,
                                            ast_pos,
                                            ErrorDisplayType::Error,
                                            vec![ErrorAnnotation::new(
                                                Some("unexpected statement".to_string()),
                                                ast_pos,
                                                ErrorDisplayType::Error,
                                            )],
                                        )
                                    }),
                                    ast_pos,
                                    true,
                                ));
                                break;
                            }
                        }
                        Err(e) => {
                            if e.urgent {
                                return Err(vec![e.into()]);
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
                    return Err(vec![LoggerInner::longest(errors).into()]);
                }
            }
        }

        self.ast = Some(ast_list);
        Ok(())
    }

    /// Parse basic block
    pub fn block(&mut self, scope: Scope) -> Result<ast::Block, ErrorGen> {
        let position = self.token_pos;
        self.validate_token(lexer::TokenType::LCP, position, false)?;

        let mut ast_list: Vec<Statement> = Vec::new();
        loop {
            let mut errors: Vec<ErrorGen> = Vec::new();
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
                            let ast_pos = ast_production.pos();
                            errors.push(ErrorGen::new(
                                Box::new(move || {
                                    ErrorValue::new(
                                        "unexpected statement in inner scope".to_string(),
                                        ErrorType::Syntax,
                                        ast_pos,
                                        ErrorDisplayType::Error,
                                        vec![ErrorAnnotation::new(
                                            Some("unexpected statement".to_string()),
                                            ast_pos,
                                            ErrorDisplayType::Error,
                                        )],
                                    )
                                }),
                                ast_pos,
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
        self.validate_token(lexer::TokenType::RCP, position, false)?;

        Ok(ast::Block {
            nodes: ast_list,
            ty: None,
            tags: UnitTags::new(),
            pos: self.get_relative_pos(position),
        })
    }

    fn type_assign(&mut self) -> Result<Statement, ErrorGen> {
        let position = self.token_pos;

        let visibility = if self.peek().token == lexer::TokenType::Public {
            self.forward();
            ast::Visibility::Public
        } else {
            ast::Visibility::Private
        };
        self.validate_token(lexer::TokenType::Type, position, true)?;

        let name = self.namespace()?;

        self.validate_token(lexer::TokenType::Equals, position, false)?;

        let value = self.type_expr()?;

        self.validate_token(lexer::TokenType::LineTerm, position, false)?;

        Ok(Statement::TypeAssign(ast::TypeAssign {
            value,
            name: Rc::new(name),
            visibility,
            pos: self.get_relative_pos(position),
        }))
    }

    fn unit(&mut self) -> Result<Statement, ErrorGen> {
        let position = self.token_pos;
        self.validate_token(lexer::TokenType::Unit, position, true)?;
        let name = self.namespace()?;

        let block = self.block(Scope::Outer)?;

        Ok(Statement::Unit(ast::Unit {
            name,
            block: block.nodes,
            pos: self.get_relative_pos(position),
        }))
    }

    fn add_file(&mut self, name: ast::Namespace) -> Result<Statement, ErrorGen> {
        let mut scopes = name.scopes;

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
            } else if import_path
                .join(get_segment!(self.sourcemap, op.pos))
                .is_dir()
            {
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
            let cloned_sourcemap = Rc::clone(&self.sourcemap);
            import_path.push(format!("{}.fl", get_segment!(cloned_sourcemap, last.pos)));
            let last_pos = last.pos;
            Err(ErrorGen::new(
                Box::new(move || {
                    ErrorValue::new(
                        "file does not exist".to_string(),
                        ErrorType::Import,
                        last_pos,
                        ErrorDisplayType::Error,
                        vec![ErrorAnnotation::new(
                            Some(format!("file `{}` does not exist", {
                                import_path.display()
                            })),
                            last.pos,
                            ErrorDisplayType::Error,
                        )],
                    )
                }),
                last_pos,
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
                    // Error in other file
                    self.logger.borrow_mut().append_errors(e);
                    return Ok(Statement::Empty(ast::Empty { pos: last.pos }));
                }
            };

            Ok(Statement::Unit(ast::Unit {
                pos: last.pos,
                name: last.into_namespace(),
                block: parser.ast.unwrap(),
            }))
        } else {
            let name_pos = name.pos;
            Err(ErrorGen::new(
                Box::new(move || {
                    ErrorValue::new(
                        "cannot declare a unit any further than file".to_string(),
                        ErrorType::Import,
                        name_pos,
                        ErrorDisplayType::Error,
                        vec![
                            ErrorAnnotation::new(
                                Some(format!(
                                    "cannot declare unit more from `{}`",
                                    import_path.display()
                                )),
                                name_pos,
                                ErrorDisplayType::Error,
                            ),
                            ErrorAnnotation::new(
                                Some("help: did you mean to alias?".to_string()),
                                name_pos,
                                ErrorDisplayType::Info,
                            ),
                        ],
                    )
                }),
                name_pos,
                true,
            ))
        }
    }

    /// Imports
    fn import(&mut self) -> Result<Statement, ErrorGen> {
        let position = self.token_pos;
        self.validate_token(lexer::TokenType::Unit, position, true)?;

        let namespace = self.namespace()?;

        self.validate_token(lexer::TokenType::LineTerm, position, false)?;

        self.add_file(namespace)
    }

    fn compiler_tag(&mut self) -> Result<Statement, ErrorGen> {
        let position = self.token_pos;

        self.validate_token(lexer::TokenType::At, position, true)?;
        self.validate_token(lexer::TokenType::LB, position, false)?;

        // For now, tags can only be ids
        let id = self.name_id()?;

        self.validate_token(lexer::TokenType::RB, position, false)?;
        self.validate_token(lexer::TokenType::LineTerm, position, false)?;

        Ok(Statement::Tag(ast::Tag {
            content: id,
            pos: self.get_relative_pos(position),
        }))
    }

    fn parse_arguments(&mut self) -> Result<ast::Arguments, ErrorGen> {
        let position = self.token_pos;
        let mut positional_args: Vec<(Rc<ast::Namespace>, ast::Type)> = Vec::new();

        loop {
            if self.peek().token == lexer::TokenType::RP {
                // No error, we've reached the end
                break;
            }

            let id = self.namespace()?;

            self.validate_token(lexer::TokenType::Colon, position, false)?;

            let arg_type = self.type_expr()?;

            positional_args.push((Rc::new(id), arg_type));
            if self.peek().token == lexer::TokenType::Comma {
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

    /// Function expression
    fn function_expr(&mut self) -> Result<Expr, ErrorGen> {
        let position = self.token_pos;

        self.validate_token(lexer::TokenType::LP, position, false)?;

        let arguments = match self.parse_arguments() {
            Ok(val) => val,
            Err(why) => {
                self.set_pos(position);
                return Err(why);
            }
        };

        self.validate_token(lexer::TokenType::RP, position, false)?;
        self.validate_token(lexer::TokenType::DoubleColon, position, false)?;

        let (return_type, block) = match self.type_expr() {
            Ok(ty) => (ty, Expr::Block(self.block(Scope::Block)?)),
            Err(_) => {
                let block = self.block(Scope::Block)?;
                (
                    ast::Type {
                        value: ast::TypeType::Unknown,
                        pos: block.pos,
                    },
                    Expr::Block(block),
                )
            }
        };

        Ok(Expr::Function(ast::Function {
            return_type,
            arguments,
            ty: None,
            block: Box::new(block),
            pos: self.get_relative_pos(position),
        }))
    }

    /// Yield expr
    fn yield_expr(&mut self) -> Result<Expr, ErrorGen> {
        let position = self.token_pos;

        self.validate_token(lexer::TokenType::Yield, position, true)?;

        let expr = self.expr(Prec::Lowest)?;

        Ok(Expr::Yield(ast::Yield {
            expression: Box::new(expr),
            pos: self.get_relative_pos(position),
        }))
    }

    /// Return expr
    fn return_expr(&mut self) -> Result<Expr, ErrorGen> {
        let position = self.token_pos;

        self.validate_token(lexer::TokenType::Return, position, true)?;

        let expr = self.expr(Prec::Lowest)?;

        Ok(Expr::Return(ast::Return {
            expression: Box::new(expr),
            pos: self.get_relative_pos(position),
        }))
    }

    /// Expressions statement
    fn expression_statement(&mut self) -> Result<Statement, ErrorGen> {
        let position = self.token_pos;
        let expr = self.expr(Prec::Lowest)?;

        self.validate_token(lexer::TokenType::LineTerm, position, false)?;

        Ok(ast::Statement::ExpressionStatement(
            ast::ExpressionStatement {
                expression: Box::new(expr),
                pos: self.get_relative_pos(position),
            },
        ))
    }

    /// Arguments of function call
    fn arguments_call(&mut self) -> Result<ast::ArgumentsRun, ErrorGen> {
        let position = self.token_pos;
        let mut positional_args: Vec<Expr> = Vec::new();

        loop {
            if self.peek().token == lexer::TokenType::RP {
                // No error, we've reached the end
                break;
            }

            let expr = self.expr(Prec::Lowest)?;

            positional_args.push(expr);
            if self.peek().token == lexer::TokenType::Comma {
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
    fn function_call(&mut self) -> Result<Expr, ErrorGen> {
        let position = self.token_pos;

        let namespace = Rc::new(self.namespace()?);

        self.validate_token(lexer::TokenType::LP, position, false)?;

        let arguments = self.arguments_call()?;

        self.validate_token(lexer::TokenType::RP, position, false)?;

        Ok(Expr::FunctionCall(ast::FunctionCall {
            arguments,
            name: namespace,
            pos: self.get_relative_pos(position),
            mangled_name: None,
            mangle: true,
        }))
    }

    /// Generic values
    fn generic(&mut self) -> Result<ast::Generics, ErrorGen> {
        let mut items = HashMap::new();
        let first_item = self.generic_item()?;
        items.insert(first_item.0, first_item.1);

        loop {
            if self.peek().token == lexer::TokenType::Comma {
                self.forward();
                // possible trailing comma
                if let Ok((ty_name, patterns)) = self.generic_item() {
                    items.insert(ty_name, patterns);
                }
            } else {
                break;
            }
        }
        Ok(ast::Generics { ty_map: items })
    }

    fn generic_item(&mut self) -> Result<(ast::Namespace, Vec<ast::Namespace>), ErrorGen> {
        let ty_name = self.namespace()?;
        let patterns = if self.peek().token == lexer::TokenType::Colon {
            self.forward();
            self.patterns()?
        } else {
            Vec::new()
        };

        Ok((ty_name, patterns))
    }

    fn patterns(&mut self) -> Result<Vec<ast::Namespace>, ErrorGen> {
        let mut patterns = vec![self.namespace()?];
        loop {
            if self.peek().token == lexer::TokenType::Add {
                self.forward();
                patterns.push(self.namespace()?);
            } else {
                break;
            }
        }
        Ok(patterns)
    }

    /// Full variable assign with type declaration and expression
    fn variable_assign_full(&mut self) -> Result<Expr, ErrorGen> {
        let position = self.token_pos;

        let visibility = if self.peek().token == lexer::TokenType::Public {
            self.forward();
            ast::Visibility::Public
        } else {
            ast::Visibility::Private
        };

        let is_extern = self.peek().token == lexer::TokenType::Extern;
        if is_extern {
            self.forward();
        }

        self.validate_token(lexer::TokenType::Let, position, true)?;

        let namespace = self.namespace()?;

        let generics = if self.peek().token == lexer::TokenType::LT {
            self.forward();
            // Generic types goes before colon
            let generics = self.generic()?;
            self.validate_token(lexer::TokenType::GT, position, false)?;
            Some(generics)
        } else {
            None
        };

        let var_type = if lexer::TokenType::Colon == self.peek().token {
            self.forward();
            self.type_expr()?
        } else {
            ast::Type {
                value: ast::TypeType::Unknown,
                pos: namespace.pos,
            }
        };

        let expr = if !is_extern {
            self.validate_token(lexer::TokenType::Equals, position, false)?;
            Some(Box::new(self.expr(Prec::Lowest)?))
        } else {
            None
        };

        Ok(Expr::VariableAssignDeclaration(
            ast::VariableAssignDeclaration {
                ty: var_type,
                typecheck_type: None,
                name: Rc::new(namespace),
                expr,
                generics,
                visibility,
                pos: self.get_relative_pos(position),
            },
        ))
    }

    fn conditional(&mut self) -> Result<Expr, ErrorGen> {
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

        Ok(Expr::Conditional(ast::Conditional {
            if_branches,
            else_branch,
            pos: self.get_relative_pos(position),
        }))
    }

    fn if_cond(&mut self) -> Result<ast::IfBranch, ErrorGen> {
        let position = self.token_pos;

        self.validate_token(lexer::TokenType::If, position, true)?;
        let cond = self.expr(Prec::Lowest)?;

        let block = self.block(Scope::Block)?;

        Ok(ast::IfBranch {
            cond,
            block,
            pos: self.get_relative_pos(position),
        })
    }

    fn else_if_cond(&mut self) -> Result<ast::IfBranch, ErrorGen> {
        let position = self.token_pos;

        self.validate_token(lexer::TokenType::Else, position, true)?;
        self.validate_token(lexer::TokenType::If, position, true)?;

        let cond = self.expr(Prec::Lowest)?;

        let block = self.block(Scope::Block)?;

        Ok(ast::IfBranch {
            cond,
            block,
            pos: self.get_relative_pos(position),
        })
    }

    fn else_cond(&mut self) -> Result<ast::ElseBranch, ErrorGen> {
        let position = self.token_pos;

        self.validate_token(lexer::TokenType::Else, position, true)?;
        let block = self.block(Scope::Block)?;

        Ok(ast::ElseBranch {
            block,
            pos: self.get_relative_pos(position),
        })
    }

    /// Top level expression
    fn expr(&mut self, prec: Prec) -> Result<Expr, ErrorGen> {
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

            match self.led(left, operator) {
                Ok(val) => {
                    left = val;
                }

                // Unmatched expression if Err(), raise error (priority mode)
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

    fn get_operator_infix(&mut self) -> Result<lexer::Token, ErrorGen> {
        let position = self.token_pos;
        let potential_op = self.forward();

        if self.infix_op.get(&potential_op.token).is_some() {
            return Ok(potential_op);
        }

        let pos = self.get_relative_pos(position);
        let temp = Err(ErrorGen::new(
            Box::new(move || {
                ErrorValue::new(
                    "Expected an operator".to_string(),
                    ErrorType::Syntax,
                    pos,
                    ErrorDisplayType::Error,
                    vec![ErrorAnnotation::new(None, pos, ErrorDisplayType::Error)],
                )
            }),
            pos,
            false,
        ));

        self.set_pos(position);
        temp
    }

    fn get_operator_prefix(&mut self) -> Result<lexer::Token, ErrorGen> {
        let position = self.token_pos;
        let potential_op = self.forward();
        if self.prefix_op.get(&potential_op.token).is_some() {
            return Ok(potential_op);
        }

        let pos = self.get_relative_pos(position);

        let temp = Err(ErrorGen::new(
            Box::new(move || {
                ErrorValue::new(
                    "Expected a prefix".to_string(),
                    ErrorType::Syntax,
                    pos,
                    ErrorDisplayType::Error,
                    vec![ErrorAnnotation::new(None, pos, ErrorDisplayType::Error)],
                )
            }),
            pos,
            false,
        ));

        self.set_pos(position);
        temp
    }

    #[inline]
    fn binding_power(&mut self, token_type: &lexer::TokenType) -> Prec {
        self.infix_op[token_type]
    }

    fn led(&mut self, left: Expr, operator: lexer::Token) -> Result<Expr, ErrorGen> {
        let position = self.token_pos;

        match operator.token {
            lexer::TokenType::As => Ok(Expr::As(ast::AsExpr {
                expr: Box::new(left),
                ty: self.type_expr()?,
                pos: self.get_relative_pos(position),
            })),
            lexer::TokenType::Is => Ok(Expr::Is(ast::IsExpr {
                expr: Box::new(left),
                ty: self.type_expr()?,
                pos: self.get_relative_pos(position),
            })),
            _ => {
                let bp = self.binding_power(&operator.token);
                Ok(Expr::Infix(ast::Infix {
                    left: Box::new(left),
                    operator,
                    right: Box::new(self.expr(bp)?),
                    pos: self.get_relative_pos(position),
                }))
            }
        }
    }

    fn item(&mut self) -> Result<Expr, ErrorGen> {
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

        self.item_single(position)
    }

    fn item_single(&mut self, position: usize) -> Result<Expr, ErrorGen> {
        run_all! {
            self,
            Parser::integer,
            Parser::string_literal,
            Parser::bool_expr,
            Parser::function_call,
            Parser::variable_assign_full,
            Parser::function_expr,
            Parser::tuple_expr,
            Parser::ref_expr,
            Parser::conditional,
            Parser::return_expr,
            Parser::yield_expr
        };

        match self.block(Scope::Block) {
            Ok(block) => return Ok(Expr::Block(block)),
            Err(_) => {}
        }

        if lexer::TokenType::LP == self.forward().token {
            let t = self.peek();
            if lexer::TokenType::RP == t.token {
                // It's (), let the user know
                let p = self.forward().pos;
                return Err(ErrorGen::new(
                    Box::new(move || {
                        ErrorValue::new(
                            "expected token `,`, found token `)`".to_string(),
                            ErrorType::Syntax,
                            p,
                            ErrorDisplayType::Error,
                            vec![ErrorAnnotation::new(
                                Some(format!("unexpected token")),
                                p,
                                ErrorDisplayType::Error,
                            )],
                        )
                        .with_note(
                            "help: if you want an empty tuple,\nadd a comma: `(,)`".to_string(),
                        )
                    }),
                    p,
                    true,
                ));
            }
            let expr = self.expr(Prec::Lowest)?;
            if lexer::TokenType::RP == self.forward().token {
                return Ok(expr);
            }
            let next_tok = self.peek();
            return Err(self.syntax_error(next_tok, lexer::TokenType::RP, false, false));
        }

        let next = self.peek();
        let pos = self.get_relative_pos(position);

        let cloned_sourcemap = Rc::clone(&self.sourcemap);

        let temp = Err(ErrorGen::new(
            Box::new(move || {
                ErrorValue::new(
                    "missing expression".to_string(),
                    ErrorType::Syntax,
                    pos,
                    ErrorDisplayType::Error,
                    vec![ErrorAnnotation::new(
                        Some(format!(
                            "expected expression, found {}",
                            next.f(Rc::clone(&cloned_sourcemap))
                        )),
                        pos,
                        ErrorDisplayType::Error,
                    )],
                )
            }),
            pos,
            false,
        ));
        self.set_pos(position);
        temp
    }

    fn bool_expr(&mut self) -> Result<Expr, ErrorGen> {
        let position = self.token_pos;

        let possible_bool = self.forward();
        if lexer::TokenType::True == possible_bool.token
            || lexer::TokenType::False == possible_bool.token
        {
            Ok(Expr::Literal(ast::Literal {
                literal_type: LiteralType::Bool,
                pos: possible_bool.pos,
            }))
        } else {
            let temp = Err(self.syntax_error(possible_bool, lexer::TokenType::True, false, false));
            self.set_pos(position);
            temp
        }
    }

    fn integer(&mut self) -> Result<Expr, ErrorGen> {
        let position = self.token_pos;

        let int = self.forward();
        if lexer::TokenType::Number == int.token {
            Ok(Expr::Literal(ast::Literal {
                literal_type: LiteralType::Number,
                pos: int.pos,
            }))
        } else {
            let temp = Err(self.syntax_error(int, lexer::TokenType::Number, false, false));
            self.set_pos(position);
            temp
        }
    }

    fn string_literal(&mut self) -> Result<Expr, ErrorGen> {
        let position = self.token_pos;

        let string = self.forward();
        if lexer::TokenType::String == string.token {
            Ok(Expr::Literal(ast::Literal {
                pos: string.pos,
                literal_type: LiteralType::String,
            }))
        } else {
            let temp = Err(self.syntax_error(string, lexer::TokenType::String, false, false));
            self.set_pos(position);
            temp
        }
    }

    fn namespace(&mut self) -> Result<ast::Namespace, ErrorGen> {
        let position = self.token_pos;
        let mut ids: ast::NamespaceInner = SmallVec::new();
        let id = self.name_id()?;

        ids.push(id);

        loop {
            if self.peek().token != lexer::TokenType::DoubleColon {
                break;
            }

            self.validate_token(lexer::TokenType::DoubleColon, position, false)?;

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
    fn name_id(&mut self) -> Result<ast::NameID, ErrorGen> {
        let position = self.token_pos;
        let id = self.forward();
        if lexer::TokenType::Identifier == id.token {
            Ok(ast::NameID {
                sourcemap: Rc::clone(&self.sourcemap),
                pos: id.pos,
            })
        } else {
            let temp = Err(self.syntax_error(id, lexer::TokenType::Identifier, false, false));
            self.set_pos(position);
            temp
        }
    }

    fn ref_expr(&mut self) -> Result<Expr, ErrorGen> {
        Ok(Expr::RefID(self.ref_id()?))
    }

    /// Parse ref identifier (i.e. variable refrence (not &))
    fn ref_id(&mut self) -> Result<ast::RefID, ErrorGen> {
        let value = self.namespace()?;
        Ok(ast::RefID {
            pos: value.pos,
            value: Rc::new(value),
        })
    }

    fn tuple_expr(&mut self) -> Result<Expr, ErrorGen> {
        Ok(Expr::Tuple(self.tuple()?))
    }

    fn tuple(&mut self) -> Result<ast::Tuple, ErrorGen> {
        let position = self.token_pos;
        self.validate_token(lexer::TokenType::LP, position, false)?;

        let values = match self.items() {
            Ok(items) => {
                if items.len() == 1 {
                    // Required trailing comma
                    self.validate_token(lexer::TokenType::Comma, position, false)?;
                } else {
                    // Optional trailing comma
                    if let lexer::TokenType::Comma = self.peek().token {
                        self.forward();
                    }
                }
                items
            }
            Err(_) => {
                // Optional trailing comma
                if let lexer::TokenType::Comma = self.peek().token {
                    self.forward();
                };
                Vec::new()
            }
        };

        self.validate_token(lexer::TokenType::RP, position, false)?;

        Ok(ast::Tuple {
            values,
            pos: self.get_relative_pos(position),
        })
    }

    fn items(&mut self) -> Result<Vec<Expr>, ErrorGen> {
        let mut items: Vec<Expr> = Vec::new();

        let expr = self.expr(Prec::Lowest)?;
        items.push(expr);

        loop {
            let position = self.token_pos;
            if let lexer::TokenType::Comma = self.peek().token {
                self.forward();
                if let Ok(expr) = self.expr(Prec::Lowest) {
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

    fn function_type(&mut self) -> Result<ast::Type, ErrorGen> {
        let position = self.token_pos;

        self.validate_token(lexer::TokenType::LP, position, false)?;

        let arguments = match self.function_type_args() {
            Ok(val) => val,
            Err(why) => {
                self.set_pos(position);
                return Err(why);
            }
        };

        self.validate_token(lexer::TokenType::RP, position, false)?;

        self.validate_token(lexer::TokenType::DoubleColon, position, false)?;
        let return_type = self.type_expr()?;

        Ok(ast::Type {
            pos: self.get_relative_pos(position),
            value: ast::TypeType::Function(arguments, Box::new(return_type)),
        })
    }

    fn function_type_args(&mut self) -> Result<Vec<ast::Type>, ErrorGen> {
        let mut positional_args: Vec<ast::Type> = Vec::new();

        loop {
            if self.peek().token == lexer::TokenType::RP {
                // No error, we've reached the end
                break;
            }

            let position = self.token_pos;
            let id = self.namespace();

            if let (Ok(_), lexer::TokenType::Colon) = (id, self.peek().token) {
                // There is a name id, eat it; its optional
                self.forward();

                let arg_type = self.type_expr()?;
                positional_args.push(arg_type);
            } else {
                // No name id, its optional so this is fine
                self.token_pos = position;
                let arg_type = self.type_expr()?;
                positional_args.push(arg_type);
            }

            if self.peek().token == lexer::TokenType::Comma {
                self.forward();
            } else {
                break;
            }
        }

        Ok(positional_args)
    }

    fn underscore_type(&mut self) -> Result<ast::Type, ErrorGen> {
        let pos = self.validate_token(lexer::TokenType::Underscore, self.token_pos, false)?;

        Ok(ast::Type {
            pos,
            value: ast::TypeType::Unknown,
        })
    }

    fn namespace_type(&mut self) -> Result<ast::Type, ErrorGen> {
        let namespace = self.namespace()?;

        Ok(ast::Type {
            pos: namespace.pos,
            value: ast::TypeType::Type(Rc::new(namespace)),
        })
    }

    fn tuple_type(&mut self) -> Result<ast::Type, ErrorGen> {
        let position = self.token_pos;

        self.validate_token(lexer::TokenType::LP, position, false)?;

        let values = match self.items_type() {
            Ok(items) => {
                if items.len() == 1 {
                    // Required trailing comma
                    if let Err(why) = self.validate_token(lexer::TokenType::Comma, position, false)
                    {
                        let pos = *&why.position;
                        return Err(ErrorGen::new(
                            Box::new(move || {
                                why.mk_err().with_note(
                                    "help: if you meant to make a tuple type, add a comma"
                                        .to_string(),
                                )
                            }),
                            pos,
                            true,
                        ));
                    }
                } else {
                    // Optional trailing comma
                    if let lexer::TokenType::Comma = self.peek().token {
                        self.forward();
                    }
                }
                items
            }
            Err(_) => {
                // Optional trailing comma
                if let lexer::TokenType::Comma = self.peek().token {
                    self.forward();
                };
                Vec::new()
            }
        };
        self.validate_token(lexer::TokenType::RP, position, false)?;

        Ok(ast::Type {
            value: ast::TypeType::Tuple(values),
            pos: self.get_relative_pos(position),
        })
    }

    fn items_type(&mut self) -> Result<Vec<ast::Type>, ErrorGen> {
        let mut items: Vec<ast::Type> = Vec::new();

        let type_type = self.type_expr()?;
        items.push(type_type);

        loop {
            let position = self.token_pos;
            if let lexer::TokenType::Comma = self.peek().token {
                self.forward();
                if let Ok(type_type) = self.type_expr() {
                    items.push(type_type);
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
    fn type_expr(&mut self) -> Result<ast::Type, ErrorGen> {
        let position = self.token_pos;

        run_all! {
            self,
            Parser::namespace_type,
            Parser::underscore_type,
            Parser::function_type,
            Parser::tuple_type
        }

        let pos = self.get_relative_pos(position);
        let cloned_sourcemap = Rc::clone(&self.sourcemap);

        let next = self.peek();

        let temp = Err(ErrorGen::new(
            Box::new(move || {
                ErrorValue::new(
                    "Missing type".to_string(),
                    ErrorType::Syntax,
                    pos,
                    ErrorDisplayType::Error,
                    vec![ErrorAnnotation::new(
                        Some(format!(
                            "Expected type, found {}",
                            next.f(Rc::clone(&cloned_sourcemap))
                        )),
                        pos,
                        ErrorDisplayType::Error,
                    )],
                )
            }),
            pos,
            false,
        ));
        self.set_pos(position);
        temp
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
                let filename_code =
                    insert_file!(sourcemap, path::PathBuf::from(FILENAME), $code.to_string());
                let logger = LoggerInner::new(true, Rc::clone(&sourcemap));
                let mut parser = Parser::new(filename_code, logger, sourcemap);
                parser.initialize_expr();
                parser.fill_token_stream()?;
                $function(&mut parser)?;
                Ok(())
            }
        };
    }

    parser_run!(
        "- 1 - 1 + 1 / 1 % 1 %% 1 * 1 as i32 > 1 is i32 < 1 >= 1 <= 1 == 1",
        move |value| Parser::expr(value, Prec::Lowest),
        expr
    );

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

    parser_run!("_", Parser::type_expr, underscore_type);

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

    parser_run!(
        "let x: (int, int) = 10",
        Parser::variable_assign_full,
        variable_assign_full
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
        "return ((let x: int = 10, hello, 1, \"another_test\"))",
        Parser::return_expr,
        return_statement
    );

    parser_run!(
        "yield ((let x: int = 10, hello, 1, \"another_test\"))",
        Parser::yield_expr,
        yield_statement
    );

    parser_run!(
        "let add_overload_test9 = (val: int, val: int) :: int {}",
        Parser::expression_statement,
        function_define
    );

    parser_run!(
        "pub let add_overload_test9 = (val: int, val: int) :: int {}",
        Parser::expression_statement,
        function_define_pub
    );

    parser_run!(
        "pub extern let test: int",
        Parser::expression_statement,
        extern_let_pub
    );

    parser_run!(
        "extern let test: int",
        Parser::expression_statement,
        extern_let
    );

    parser_run!(
        "extern let test: (i32, i32) :: i32",
        Parser::expression_statement,
        extern_let_func
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

    parser_run!("a(10, 10, \"hello\")", Parser::item, call_expr_1_item);
    parser_run!("a(10, 10,)", Parser::item, call_expr_2_item);
    parser_run!("(a(10, 10, \"hello\"))", Parser::item, call_expr_1_paren);
    parser_run!("(a(10, 10,))", Parser::item, call_expr_2_paren);

    parser_run!("(let x: int = 5)", Parser::item, variable_assign_full_paren);
    parser_run!("let x: int = 5", Parser::item, variable_assign_full_item);

    parser_run!("let x = 5", Parser::item, variable_assign_full_item_no_type);
    parser_run!(
        "let x = 5",
        Parser::expression_statement,
        variable_assign_full_item_no_type_stmt
    );

    parser_run!(
        "let x<Ty: X> = 5",
        Parser::item,
        variable_assign_generic_with_bound
    );
    parser_run!(
        "let x<X> = 5",
        Parser::item,
        variable_assign_generic_without_bound
    );
    parser_run!(
        "(let x<Ty: X> = 5)",
        Parser::item,
        variable_assign_generic_with_bound_paren
    );
    parser_run!(
        "(let x<X> = 5)",
        Parser::item,
        variable_assign_generic_without_bound_paren
    );

    parser_run!("hi: hi + hi, hi: hi", Parser::generic, generic_complex);
    parser_run!("hi: hi", Parser::generic, generic_simple);
    parser_run!(
        "hi: hi + hi, hi: hi,",
        Parser::generic,
        generic_complex_trailing
    );
    parser_run!("hi: hi,", Parser::generic, generic_simple_trailing);

    parser_run!("(i)", Parser::item, ref_id_paren);
    parser_run!("i3_hello_world", Parser::item, ref_id_item);

    parser_run!(
        "(let x: int = 10, hello, 1, \"another_test\")",
        Parser::item,
        tuple_item
    );
    parser_run!(
        "((let x: int = 10, hello, 1, \"another_test\"))",
        Parser::item,
        tuple_paren
    );

    parser_run!("(19)", Parser::item, int_1_paren_stmt);
    parser_run!("(1)", Parser::item, int_2_paren_stmt);
    parser_run!("1", Parser::item, int_1_stmt);
    parser_run!("19", Parser::item, int_2_stmt);

    parser_run!("(true)", Parser::item, bool_true_paren_stmt);
    parser_run!("(false)", Parser::item, bool_false_paren_stmt);
    parser_run!("true", Parser::item, bool_true_item_stmt);
    parser_run!("false", Parser::item, bool_false_item_stmt);

    parser_run!("\"dab\"", Parser::item, string_literal_1_item_stmt);
    parser_run!("\"\"", Parser::item, string_literal_2_item_stmt);
    parser_run!("(\"dab\")", Parser::item, string_literal_1_paren_stmt);
    parser_run!("(\"\")", Parser::item, string_literal_2_paren_stmt);

    parser_run!("a(10, 10, \"hello\")", Parser::item, call_expr_1_item_stmt);
    parser_run!("a(10, 10,)", Parser::item, call_expr_2_item_stmt);
    parser_run!(
        "(a(10, 10, \"hello\"))",
        Parser::item,
        call_expr_1_paren_stmt
    );
    parser_run!("(a(10, 10,))", Parser::item, call_expr_2_paren_stmt);

    parser_run!("{}", Parser::expression_statement, block_expr_stmt_1);
    parser_run!(
        "{yield let x = 5}",
        Parser::expression_statement,
        block_expr_stmt_2
    );

    parser_run!(
        "(let x: int = 5)",
        Parser::item,
        variable_assign_full_paren_stmt
    );
    parser_run!(
        "let x: int = 5",
        Parser::item,
        variable_assign_full_item_stmt
    );

    parser_run!("(i)", Parser::item, ref_id_paren_stmt);
    parser_run!("i3_hello_world", Parser::item, ref_id_item_stmt);

    parser_run!(
        "(let x: int = 10, hello, 1, \"another_test\")",
        Parser::item,
        tuple_item_stmt
    );
    parser_run!(
        "((let x: int = 10, hello, 1, \"another_test\"))",
        Parser::item,
        tuple_paren_stmt
    );

    macro_rules! parser_err {
        ($code: expr, $function: expr, $name: ident) => {
            #[test]
            fn $name() -> Result<(), ErrorValue> {
                let sourcemap = SourceMapInner::new();
                let filename_code =
                    insert_file!(sourcemap, path::PathBuf::from(FILENAME), $code.to_string());
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

    parser_err!("hi: ", Parser::generic, generic_simple_other_fail);
    parser_err!("hi: hi +", Parser::generic, generic_simple_fail);

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
}
