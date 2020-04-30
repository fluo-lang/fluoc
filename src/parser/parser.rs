use crate::codegen::module_codegen::CodeGenModule;
use crate::helpers;
use crate::lexer;
use crate::logger::logger::{Error, ErrorAnnotation, ErrorDisplayType, ErrorType, Logger};
use crate::parser::ast;
use crate::parser::ast::{Expr, Scope, Statement};

use std::collections::HashMap;

macro_rules! ignore_or_return {
    ( $e:expr ) => {
        match $e {
            Ok(x) => return Ok(x),
            Err(_) => {}
        }
    };
}

#[derive(Copy, Clone)]
pub enum Prec {
    LOWEST = 0,
    // TODO: Add bool operators
    /// `+` and `-`
    TERM = 1,

    /// `*` and `/` and `%`
    FACTOR = 2,

    /// `-` (negate) and others (i.e. `!` logical negate)
    PREFIX = 3,

    /// Function call
    CALL = 4,

    /// Variable assignment as expr
    VARIABLE = 5,
}

/// Recursive descent parser
pub struct Parser<'a> {
    /// Lexer object
    pub lexer: lexer::Lexer<'a>,
    /// Abstract syntax tree
    pub ast: Option<ast::Block<'a>>,
    pub modules: HashMap<ast::Namespace<'a>, CodeGenModule<'a>>,
    statements: Vec<fn(&mut Self) -> Result<Statement<'a>, Error<'a>>>,
    prefix_op: HashMap<lexer::TokenType<'a>, Prec>,
    infix_op: HashMap<lexer::TokenType<'a>, Prec>,
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
            statements: vec![
                Parser::function_define,
                Parser::expression_statement,
                Parser::return_statement,
                Parser::variable_declaration,
            ],
            prefix_op: HashMap::new(),
            infix_op: HashMap::new(),
        }
    }
    /// Template for syntax error
    pub fn syntax_error(
        &self,
        t: lexer::Token,
        message: &str,
        is_keyword: bool,
        urgent: bool,
    ) -> Error<'a> {
        Error::new(
            String::from(if !is_keyword {
                format!("{}, found {}", message, t)
            } else {
                format!("unexpected {}", t)
            }),
            ErrorType::Syntax,
            t.pos,
            ErrorDisplayType::Error,
            self.lexer.filename,
            vec![ErrorAnnotation::new(
                Some("unexpected token".to_string()),
                t.pos,
                ErrorDisplayType::Error,
                self.lexer.filename,
            )],
            urgent,
        )
    }

    /// Validate next token
    pub fn next(
        &mut self,
        token_type: lexer::TokenType,
        position: (usize, usize),
        is_keyword: bool,
    ) -> Result<(), Error<'a>> {
        let t = self.lexer.advance()?;

        if t.token != token_type {
            self.lexer.set_pos(position);
            let error = format!("expected {}", token_type);
            Err(self.syntax_error(t, &error[..], is_keyword, false))
        } else {
            Ok(())
        }
    }

    pub fn position(&mut self, position: (usize, usize)) -> helpers::Pos {
        helpers::Pos {
            s: position.0,
            e: self.lexer.position,
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
    }

    pub fn register_prefix(&mut self, token: lexer::TokenType<'a>, prec: Prec) {
        self.prefix_op.insert(token, prec);
    }

    pub fn register_infix(&mut self, token: lexer::TokenType<'a>, prec: Prec) {
        self.infix_op.insert(token, prec);
    }

    /// Parse from lexer
    ///
    /// Returns nothing
    pub fn parse(&mut self) -> Result<(), Vec<Error<'a>>> {
        let position = match self.lexer.get_pos() {
            Ok(t) => t,
            Err(e) => {
                return Err(vec![e]);
            }
        };

        // Set our scope to outside
        let scope = Scope::Outer;

        let mut ast_list: Vec<Statement> = Vec::new();
        let next = self.lexer.peek();
        match next {
            Err(e) => Err(vec![e]),
            Ok(next) => {
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
                                        errors.push(Error::new(
                                            "unexpected statement in outer scope".to_string(),
                                            ErrorType::Syntax,
                                            ast_production.pos(),
                                            ErrorDisplayType::Error,
                                            self.lexer.filename,
                                            vec![ErrorAnnotation::new(
                                                Some("unexpected statement".to_string()),
                                                ast_production.pos(),
                                                ErrorDisplayType::Error,
                                                self.lexer.filename,
                                            )],
                                            true,
                                        ));
                                        break;
                                    }
                                }
                                Err(e) => errors.push(e),
                            }
                        }
                        let temp_peek = self.lexer.peek();
                        match temp_peek {
                            Err(e) => {
                                return Err(vec![e]);
                            }
                            Ok(temp_peek) => {
                                if temp_peek.token == lexer::TokenType::EOF && !fail {
                                    // We've successfully parsed, break
                                    break;
                                } else if !errors.is_empty() && fail {
                                    // We've found an error, raise the error
                                    return Err(vec![Logger::longest(errors)]);
                                }
                            }
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
        }
    }

    /// Parse basic block
    pub fn block(&mut self) -> Result<ast::Block<'a>, Error<'a>> {
        let position = self.lexer.get_pos()?;
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
                                self.lexer.filename,
                                vec![ErrorAnnotation::new(
                                    Some("unexpected statement".to_string()),
                                    ast_production.pos(),
                                    ErrorDisplayType::Error,
                                    self.lexer.filename,
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

            if self.lexer.peek()?.token == lexer::TokenType::RCP && !fail {
                // We've successfully parsed, break
                break;
            } else if !errors.is_empty() && fail {
                // We've found an error, return the error
                return Err(Logger::longest(errors));
            } else if self.lexer.peek()?.token != lexer::TokenType::RCP && fail {
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

    pub fn parse_arguments(&mut self) -> Result<ast::Arguments<'a>, Error<'a>> {
        let position = self.lexer.get_pos()?;
        let mut positional_args: Vec<(ast::NameID, ast::Type)> = Vec::new();

        loop {
            if self.lexer.peek()?.token == lexer::TokenType::RP {
                // No error, we've reached the end
                break;
            }

            let id = self.name_id()?;

            self.next(lexer::TokenType::COLON, position, false)?;

            let arg_type = self.type_expr()?;

            positional_args.push((id, arg_type));
            if self.lexer.peek()?.token == lexer::TokenType::COMMA {
                self.lexer.advance()?;
            } else {
                break;
            }
        }

        Ok(ast::Arguments {
            positional: positional_args,
            pos: self.position(position),
        })
    }

    /// Parse function definition
    pub fn function_define(&mut self) -> Result<Statement<'a>, Error<'a>> {
        let position = self.lexer.get_pos()?;
        self.next(lexer::TokenType::DEF, position, true)?;

        let id = self.name_id()?;

        self.next(lexer::TokenType::LP, position, false)?;

        let arguments = self.parse_arguments()?;

        self.next(lexer::TokenType::RP, position, false)?;

        let return_type: ast::Type;

        if self.lexer.peek()?.token == lexer::TokenType::ARROW {
            self.lexer.advance()?;
            return_type = self.type_expr()?;
        } else {
            return_type = ast::Type {
                value: ast::TypeType::Tuple(Vec::new()),
                inferred: true,
                pos: helpers::Pos {
                    s: self.lexer.position,
                    e: self.lexer.position,
                },
            };
        }

        let block = self.block()?;
        Ok(ast::Statement::FunctionDefine(ast::FunctionDefine {
            return_type,
            arguments,
            block,
            name: id,
            pos: self.position(position),
        }))
    }

    pub fn return_statement(&mut self) -> Result<Statement<'a>, Error<'a>> {
        let position = self.lexer.get_pos()?;

        self.next(lexer::TokenType::RETURN, position, true)?;

        let expr = self.expr(Prec::LOWEST)?;
        self.next(lexer::TokenType::SEMI, position, false)?;

        Ok(ast::Statement::Return(ast::Return {
            expression: expr,
            pos: self.position(position),
        }))
    }

    /// Expressions statement
    pub fn expression_statement(&mut self) -> Result<Statement<'a>, Error<'a>> {
        let position = self.lexer.get_pos()?;

        let expr = self.expr(Prec::LOWEST)?;

        self.next(lexer::TokenType::SEMI, position, false)?;

        Ok(ast::Statement::ExpressionStatement(
            ast::ExpressionStatement {
                expression: expr,
                pos: self.position(position),
            },
        ))
    }

    pub fn arguments_call(&mut self) -> Result<ast::ArgumentsRun<'a>, Error<'a>> {
        let position = self.lexer.get_pos()?;
        let mut positional_args: Vec<Expr> = Vec::new();

        loop {
            if self.lexer.peek()?.token == lexer::TokenType::RP {
                // No error, we've reached the end
                break;
            }

            let expr = self.expr(Prec::LOWEST)?;

            positional_args.push(expr);
            if self.lexer.peek()?.token == lexer::TokenType::COMMA {
                self.lexer.advance()?;
            } else {
                break;
            }
        }

        Ok(ast::ArgumentsRun {
            positional: positional_args,
            pos: self.position(position),
        })
    }

    pub fn function_call(&mut self) -> Result<Expr<'a>, Error<'a>> {
        let position = self.lexer.get_pos()?;

        let namespace = self.namespace()?;

        self.next(lexer::TokenType::LP, position, false)?;

        let arguments = self.arguments_call()?;

        self.next(lexer::TokenType::RP, position, false)?;

        Ok(ast::Expr::FunctionCall(ast::FunctionCall {
            arguments,
            name: namespace,
            pos: self.position(position),
        }))
    }

    /// Ful variable assign with type declaration and expression
    pub fn variable_assign_full(&mut self) -> Result<Expr<'a>, Error<'a>> {
        let position = self.lexer.get_pos()?;
        self.next(lexer::TokenType::LET, position, true)?;

        let namespace = self.namespace()?;

        self.next(lexer::TokenType::COLON, position, false)?;
        let var_type = self.type_expr()?;

        self.next(lexer::TokenType::EQUALS, position, false)?;
        let expr = self.expr(Prec::LOWEST)?;

        Ok(ast::Expr::VariableAssignDeclaration(
            ast::VariableAssignDeclaration {
                t: var_type,
                name: namespace,
                expr: Box::new(expr),
                pos: self.position(position),
            },
        ))
    }

    /// Variable Declaration
    pub fn variable_declaration(&mut self) -> Result<Statement<'a>, Error<'a>> {
        let position = self.lexer.get_pos()?;
        self.next(lexer::TokenType::LET, position, true)?;

        let namespace = self.namespace()?;

        self.next(lexer::TokenType::COLON, position, false)?;
        let var_type = self.type_expr()?;

        self.next(lexer::TokenType::SEMI, position, false)?;

        Ok(ast::Statement::VariableDeclaration(
            ast::VariableDeclaration {
                t: var_type,
                name: namespace,
                pos: self.position(position),
            },
        ))
    }

    /// Variable assign with only expression
    pub fn variable_assign(&mut self) -> Result<Expr<'a>, Error<'a>> {
        let position = self.lexer.get_pos()?;

        let namespace = self.namespace()?;

        self.next(lexer::TokenType::EQUALS, position, false)?;
        let expr = self.expr(Prec::LOWEST)?;

        Ok(ast::Expr::VariableAssign(ast::VariableAssign {
            name: namespace,
            expr: Box::new(expr),
            pos: self.position(position),
        }))
    }

    /// Top level expression
    pub fn expr(&mut self, prec: Prec) -> Result<Expr<'a>, Error<'a>> {
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
            left = self.led(left, operator)?;
            if binding_power > prec as u8 {
                break;
            }
        }

        Ok(left)
    }

    fn get_operator_infix(&mut self) -> Result<lexer::Token<'a>, Error<'a>> {
        let position = self.lexer.get_pos()?;
        let potential_op = self.lexer.advance()?;
        for (op, _) in &self.infix_op {
            if &potential_op.token == op {
                return Ok(potential_op);
            }
        }

        self.lexer.set_pos(position);

        Err(Error::new(
            "Expected an operator".to_string(),
            ErrorType::Syntax,
            self.position(position),
            ErrorDisplayType::Error,
            self.lexer.filename,
            vec![ErrorAnnotation::new(
                None,
                self.position(position),
                ErrorDisplayType::Error,
                self.lexer.filename,
            )],
            false,
        ))
    }

    fn get_operator_prefix(&mut self) -> Result<lexer::Token<'a>, Error<'a>> {
        let position = self.lexer.get_pos()?;
        let potential_op = self.lexer.advance()?;
        for (op, _) in &self.prefix_op {
            if &potential_op.token == op {
                return Ok(potential_op);
            }
        }

        self.lexer.set_pos(position);

        Err(Error::new(
            "Expected a prefix".to_string(),
            ErrorType::Syntax,
            self.position(position),
            ErrorDisplayType::Error,
            self.lexer.filename,
            vec![ErrorAnnotation::new(
                None,
                self.position(position),
                ErrorDisplayType::Error,
                self.lexer.filename,
            )],
            false,
        ))
    }

    fn binding_power(&mut self, token_type: &lexer::TokenType<'a>) -> Prec {
        self.infix_op[token_type]
    }

    pub fn led(
        &mut self,
        left: Expr<'a>,
        operator: lexer::Token<'a>,
    ) -> Result<Expr<'a>, Error<'a>> {
        let position = self.lexer.get_pos()?;
        let binding_power = self.binding_power(&operator.token);

        Ok(Expr::Infix(ast::Infix {
            left: Box::new(left),
            operator,
            right: Box::new(self.expr(binding_power)?),
            pos: self.position(position),
        }))
    }

    pub fn item(&mut self) -> Result<Expr<'a>, Error<'a>> {
        let position = self.lexer.get_pos()?;

        if let Ok(prefix) = self.get_operator_prefix() {
            let bp = self.binding_power(&prefix.token);
            let item = self.expr(bp)?;
            return Ok(Expr::Prefix(ast::Prefix {
                operator: prefix,
                val: Box::new(item),
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

        if let lexer::TokenType::LP = self.lexer.advance()?.token {
            let expr = self.expr(Prec::LOWEST)?;
            if let lexer::TokenType::RP = self.lexer.advance()?.token {
                return Ok(expr);
            }
            let next_tok = self.lexer.peek()?;
            return Err(self.syntax_error(next_tok, "expected `)`", false, false));
        }

        self.lexer.set_pos(position);
        Err(Error::new(
            "Missing expression".to_string(),
            ErrorType::Syntax,
            self.position(position),
            ErrorDisplayType::Error,
            self.lexer.filename,
            vec![ErrorAnnotation::new(
                Some("Missing expression here".to_string()),
                self.position(position),
                ErrorDisplayType::Error,
                self.lexer.filename,
            )],
            false,
        ))
    }

    pub fn integer(&mut self) -> Result<Expr<'a>, Error<'a>> {
        let position = self.lexer.get_pos()?;

        let int = self.lexer.advance()?;
        if let lexer::TokenType::NUMBER(value) = &int.token {
            Ok(ast::Expr::Integer(ast::Integer {
                value: value,
                pos: int.pos,
            }))
        } else {
            self.lexer.set_pos(position);
            Err(self.syntax_error(int, "expected integer", false, false))
        }
    }

    pub fn string_literal(&mut self) -> Result<Expr<'a>, Error<'a>> {
        let position = self.lexer.get_pos()?;

        let string = self.lexer.advance()?;
        if let lexer::TokenType::STRING(value) = &string.token {
            Ok(ast::Expr::StringLiteral(ast::StringLiteral {
                value: value,
                pos: string.pos,
            }))
        } else {
            self.lexer.set_pos(position);
            Err(self.syntax_error(string, "expected string", false, false))
        }
    }

    pub fn namespace(&mut self) -> Result<ast::Namespace<'a>, Error<'a>> {
        let position = self.lexer.get_pos()?;
        let mut ids: Vec<ast::NameID> = Vec::new();
        let id = self.name_id()?;

        ids.push(id);

        loop {
            if self.lexer.peek()?.token != lexer::TokenType::DOUBLECOLON {
                break;
            }

            self.next(lexer::TokenType::DOUBLECOLON, position, false)?;

            match self.name_id() {
                Ok(id) => {
                    ids.push(id);
                }
                Err(e) => {
                    self.lexer.set_pos(position);
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
    pub fn name_id(&mut self) -> Result<ast::NameID<'a>, Error<'a>> {
        let position = self.lexer.get_pos()?;
        let id = self.lexer.advance()?;
        if let lexer::TokenType::IDENTIFIER(value) = &id.token {
            Ok(ast::NameID {
                value: value,
                pos: id.pos,
            })
        } else {
            self.lexer.set_pos(position);
            Err(self.syntax_error(id, "expected identifier", false, false))
        }
    }

    pub fn ref_expr(&mut self) -> Result<Expr<'a>, Error<'a>> {
        Ok(Expr::RefID(self.ref_id()?))
    }

    /// Parse ref identifier (i.e. variable refrence (not &))
    pub fn ref_id(&mut self) -> Result<ast::RefID<'a>, Error<'a>> {
        let position = self.lexer.get_pos()?;
        let id = self.lexer.advance()?;
        if let lexer::TokenType::IDENTIFIER(value) = &id.token {
            Ok(ast::RefID {
                value: value,
                pos: id.pos,
            })
        } else {
            self.lexer.set_pos(position);
            Err(self.syntax_error(id, "expected variable", false, false))
        }
    }

    pub fn tuple_expr(&mut self) -> Result<Expr<'a>, Error<'a>> {
        Ok(Expr::Tuple(self.tuple()?))
    }

    pub fn tuple(&mut self) -> Result<ast::Tuple<'a>, Error<'a>> {
        let position = self.lexer.get_pos()?;

        self.next(lexer::TokenType::LP, position, false)?;

        let values = match self.items() {
            Ok(items) => {
                if items.len() == 1 {
                    // Required trailing comma
                    self.next(lexer::TokenType::COMMA, position, false)?;
                } else {
                    // Optional trailing comma
                    if let lexer::TokenType::COMMA = self.lexer.peek()?.token {
                        self.lexer.advance()?;
                    }
                }
                items
            }
            Err(_) => Vec::new(),
        };
        self.next(lexer::TokenType::RP, position, false)?;

        Ok(ast::Tuple {
            values,
            pos: self.position(position),
        })
    }

    fn items(&mut self) -> Result<Vec<Expr<'a>>, Error<'a>> {
        let mut items: Vec<Expr> = Vec::new();

        let expr = self.expr(Prec::LOWEST)?;
        items.push(expr);

        loop {
            let position = self.lexer.get_pos()?;
            if let lexer::TokenType::COMMA = self.lexer.peek()?.token {
                self.lexer.advance()?;
                if let Ok(expr) = self.expr(Prec::LOWEST) {
                    items.push(expr);
                } else {
                    self.lexer.set_pos(position);
                    break;
                }
            } else {
                break;
            }
        }

        Ok(items)
    }

    /// Parse type expression
    pub fn type_expr(&mut self) -> Result<ast::Type<'a>, Error<'a>> {
        let position = self.lexer.get_pos()?;
        let mut errors: Vec<Error> = Vec::new();

        match self.namespace_type() {
            Ok(namespace) => return Ok(namespace),
            Err(e) => errors.push(e),
        }

        match self.tuple_type() {
            Ok(tuple) => return Ok(tuple),
            Err(e) => errors.push(e),
        }
        self.lexer.set_pos(position);
        Err(Logger::longest(errors))
    }

    fn namespace_type(&mut self) -> Result<ast::Type<'a>, Error<'a>> {
        let namespace = self.namespace()?;

        Ok(ast::Type {
            pos: namespace.pos,
            inferred: false,
            value: ast::TypeType::Type(namespace),
        })
    }

    fn tuple_type(&mut self) -> Result<ast::Type<'a>, Error<'a>> {
        let position = self.lexer.get_pos()?;

        self.next(lexer::TokenType::LP, position, false)?;

        let values = match self.items_type() {
            Ok(items) => {
                if items.len() == 1 {
                    // Required trailing comma
                    self.next(lexer::TokenType::COMMA, position, false)?;
                } else {
                    // Optional trailing comma
                    if let lexer::TokenType::COMMA = self.lexer.peek()?.token {
                        self.lexer.advance()?;
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

    fn items_type(&mut self) -> Result<Vec<ast::Type<'a>>, Error<'a>> {
        let mut items: Vec<ast::Type> = Vec::new();

        let type_type = self.type_expr()?;
        items.push(type_type);

        loop {
            let position = self.lexer.get_pos()?;
            if let lexer::TokenType::COMMA = self.lexer.peek()?.token {
                self.lexer.advance()?;
                if let Ok(type_type) = self.type_expr() {
                    items.push(type_type);
                } else {
                    self.lexer.set_pos(position);
                    break;
                }
            } else {
                break;
            }
        }

        Ok(items)
    }

    pub fn dollar_expr(&mut self) -> Result<Expr<'a>, Error<'a>> {
        Ok(Expr::DollarID(self.dollar_id()?))
    }

    /// Parse dollar id
    pub fn dollar_id(&mut self) -> Result<ast::DollarID<'a>, Error<'a>> {
        let position = self.lexer.get_pos()?;

        self.next(lexer::TokenType::DOLLAR, position, false)?;
        let id = self.name_id()?;

        Ok(ast::DollarID {
            value: id,
            pos: self.position(position),
        })
    }
}
