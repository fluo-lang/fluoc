use crate::codegen::module_codegen::CodeGenModule;
use crate::helpers;
use crate::lexer;
use crate::logger::logger::{Error, ErrorAnnotation, ErrorDisplayType, ErrorType, Logger};
use crate::parser::ast;
use crate::parser::ast::{Expr, Scope, Statement};
use crate::typecheck::ast_typecheck::TypeCheckType;

use std::collections::HashMap;
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
    // TODO: Add bool operators
    /// `+` and `-`
    TERM = 1,

    /// `*` and `/` and `%`
    FACTOR = 2,

    /// `as` operator (CONVersion operator)
    CONV = 3,

    /// `-` (negate) and others (i.e. `!` logical negate)
    PREFIX = 4,

    /// Function call
    CALL = 5,

    /// Variable assignment as expr
    VARIABLE = 6,
}

/// Recursive descent parser
pub struct Parser<'a> {
    /// Lexer object
    pub lexer: lexer::Lexer<'a>,
    /// Abstract syntax tree
    pub ast: Option<ast::Block<'a>>,
    pub modules: HashMap<ast::Namespace<'a>, CodeGenModule<'a>>,
    statements: [fn(&mut Self) -> Result<Statement<'a>, Error<'a>>; 5],
    prefix_op: HashMap<lexer::TokenType<'a>, Prec>,
    infix_op: HashMap<lexer::TokenType<'a>, Prec>,
    tokens: Vec<lexer::Token<'a>>,
    token_pos: usize,
}

impl<'a> Parser<'a> {
    /// Return a new parser object.
    ///
    /// Arguments
    ///
    /// * `l`: lexer to use
    /// * `log`: logger to use
    pub fn new(filename: &'a str, file_contents: &'a str) -> Parser<'a> {
        let l = lexer::Lexer::new(filename, file_contents);

        Parser {
            lexer: l,
            ast: None,
            modules: HashMap::new(),
            statements: [
                Parser::function_define,
                Parser::expression_statement,
                Parser::return_statement,
                Parser::variable_declaration,
                Parser::type_assign,
            ],
            prefix_op: HashMap::new(),
            infix_op: HashMap::new(),
            tokens: Vec::new(),
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
        token_type: lexer::TokenType,
        position: usize,
        is_keyword: bool,
    ) -> Result<(), Error<'a>> {
        let t = self.forward();

        if t.token != token_type {
            let error = format!("expected {}", token_type);
            let temp = Err(self.syntax_error(t, &error[..], is_keyword, false));
            self.set_pos(position);
            temp
        } else {
            Ok(())
        }
    }

    pub fn position(&mut self, position: usize) -> helpers::Pos<'a> {
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

    /// Parse from lexer
    ///
    /// Returns nothing
    pub fn parse(&mut self) -> Result<(), Vec<Error<'a>>> {
        if let Err(e) = self.fill_token_stream() {
            return Err(vec![e]);
        }

        let position = self.token_pos;

        // Set our scope to outside
        let scope = Scope::Outer;

        let mut ast_list: Vec<Statement> = Vec::new();
        let next = self.peek();
        if next.token != lexer::TokenType::EOF {
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
                                break;
                            } else {
                                println!("{:?}", ast_production.pos());
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
            pos: self.position(position),
        };
        self.ast = Some(block);
        Ok(())
    }

    /// Parse basic block
    pub fn block(&mut self) -> Result<ast::Block<'a>, Error<'a>> {
        let position = self.token_pos;
        self.next(lexer::TokenType::LCP, position, false)?;

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
            pos: self.position(position),
        })
    }

    fn parse_arguments(&mut self) -> Result<ast::Arguments<'a>, Error<'a>> {
        let position = self.token_pos;
        let mut positional_args: Vec<(ast::NameID, ast::TypeCheckOrType)> = Vec::new();

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
            pos: self.position(position),
        })
    }

    fn type_assign(&mut self) -> Result<Statement<'a>, Error<'a>> {
        let position = self.token_pos;
        self.next(lexer::TokenType::TYPE, position, true)?;

        let name = self.namespace()?;

        self.next(lexer::TokenType::EQUALS, position, false)?;

        let value = self.type_expr()?;

        self.next(lexer::TokenType::SEMI, position, false)?;

        Ok(Statement::TypeAssign(ast::TypeAssign {
            value: ast::TypeCheckOrType::Type(Rc::new(value)),
            name: Rc::new(name),
            pos: self.position(position),
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
        let id = self.name_id()?;

        self.next(lexer::TokenType::LP, position, false)?;

        let arguments = self.parse_arguments()?;

        self.next(lexer::TokenType::RP, position, false)?;

        let block;
        let return_type: ast::Type = if self.peek().token == lexer::TokenType::ARROW {
            self.forward();
            let temp = self.type_expr()?;
            block = self.block()?;
            temp
        } else if {
            block = self.block()?;
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
            block,
            visibility,
            name: Rc::new(id),
            pos: self.position(position),
        }))
    }

    fn return_statement(&mut self) -> Result<Statement<'a>, Error<'a>> {
        let position = self.token_pos;

        self.next(lexer::TokenType::RETURN, position, true)?;

        let expr = self.expr(Prec::LOWEST)?;
        self.next(lexer::TokenType::SEMI, position, false)?;

        Ok(ast::Statement::Return(ast::Return {
            expression: Box::new(expr),
            pos: self.position(position),
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
                pos: self.position(position),
            },
        ))
    }

    fn arguments_call(&mut self) -> Result<ast::ArgumentsRun<'a>, Error<'a>> {
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
            pos: self.position(position),
        })
    }

    fn function_call(&mut self) -> Result<Expr<'a>, Error<'a>> {
        let position = self.token_pos;

        let namespace = self.namespace()?;

        self.next(lexer::TokenType::LP, position, false)?;

        let arguments = self.arguments_call()?;

        self.next(lexer::TokenType::RP, position, false)?;

        Ok(ast::Expr::FunctionCall(ast::FunctionCall {
            arguments,
            name: Rc::new(namespace),
            pos: self.position(position),
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
                pos: self.position(position),
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
                pos: self.position(position),
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
            pos: self.position(position),
        }))
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
            self.position(position),
            ErrorDisplayType::Error,
            vec![ErrorAnnotation::new(
                None,
                self.position(position),
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
            self.position(position),
            ErrorDisplayType::Error,
            vec![ErrorAnnotation::new(
                None,
                self.position(position),
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
            type_val: None,
            pos: self.position(position),
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
                pos: self.position(position),
            }));
        }

        let exprs = [
            Parser::integer,
            Parser::string_literal,
            Parser::dollar_expr,
            Parser::function_call,
            Parser::variable_assign,
            Parser::variable_assign_full,
            Parser::ref_expr,
            Parser::tuple_expr,
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
            self.position(position),
            ErrorDisplayType::Error,
            vec![ErrorAnnotation::new(
                Some(format!("Expected expression, found {}", next)),
                self.position(position),
                ErrorDisplayType::Error,
            )],
            false,
        ));
        self.set_pos(position);
        temp
    }

    fn integer(&mut self) -> Result<Expr<'a>, Error<'a>> {
        let position = self.token_pos;

        let int = self.forward();
        if let lexer::TokenType::NUMBER(value) = &int.token {
            Ok(ast::Expr::Literal(ast::Literal {
                value,
                type_val: Some(TypeCheckType::construct_basic("int", int.pos)),
                pos: int.pos,
            }))
        } else {
            let temp = Err(self.syntax_error(int, "expected integer", false, false));
            self.set_pos(position);
            temp
        }
    }

    fn string_literal(&mut self) -> Result<Expr<'a>, Error<'a>> {
        let position = self.token_pos;

        let string = self.forward();
        if let lexer::TokenType::STRING(value) = &string.token {
            Ok(ast::Expr::Literal(ast::Literal {
                value,
                type_val: Some(TypeCheckType::construct_basic("str", string.pos)),
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
            pos: self.position(position),
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
            Err(_) => Vec::new(),
        };
        self.next(lexer::TokenType::RP, position, false)?;

        Ok(ast::Tuple {
            values,
            type_val: None,
            pos: self.position(position),
        })
    }

    fn items(&mut self) -> Result<Vec<Expr<'a>>, Error<'a>> {
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
    fn type_expr(&mut self) -> Result<ast::Type<'a>, Error<'a>> {
        let position = self.token_pos;
        let mut errors: Vec<Error> = Vec::new();

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
            Err(_) => Vec::new(),
        };
        self.next(lexer::TokenType::RP, position, false)?;

        Ok(ast::Type {
            value: ast::TypeType::Tuple(values),
            inferred: false,
            pos: self.position(position),
        })
    }

    fn items_type(&mut self) -> Result<Vec<Rc<ast::Type<'a>>>, Error<'a>> {
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
            pos: self.position(position),
        })
    }
}
