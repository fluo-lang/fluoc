use std::collections::HashMap;
use crate::parser::ast::{ NameID, Node };
use crate::logger::logger::Error;
use crate::parser::parser::Parser;
use crate::parser::ast;
use crate::helpers;
use std::fmt;

pub enum NamespaceObj {
    ParserNamespace { objects: HashMap<NameID, NamespaceObj> },
    ParserRule(fn (&mut Parser) -> Result<Node, Error>)
}

#[derive(Debug)]
/// User defined syntax ast
/// Also has closures for parsing + evaluation
pub struct Custom {
    /// Type of syntax, i.e. syntax::statement::statement
    pub custom_type: ast::Namespace,

    /// Values, i.e. { $left = ast::Integer(10), $right = ast::Integer(1) }
    pub values: HashMap<NameID, Node>,
    pub name: NameID,
    pub pos: helpers::Pos,

    /// String representation of statement
    pub rep: String,

    /// Scopes which it can be used
    pub scope: ast::Scope,
}


/// Impl block (for changeable syntax at compile time definition)
pub struct Impl {
    pub name: NameID,
    pub syntax_type: ast::Namespace,
    pub patterns: Vec<Pattern>,
    pub pos: helpers::Pos,

    /// Closures
    /// Should have `parse`, `eval`, and ``
    pub operations: Option<HashMap<String, fn (&mut Parser) -> Result<Node, Error>>>
}

impl fmt::Display for Impl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f, 
            "Impl {{\n{:#?}\n{}\n{:#?}\n{:#?}\n{:?}\n}}", 
            self.name, 
            self.syntax_type, 
            self.patterns, 
            self.pos, 
            if let Some(operations) = &self.operations {
                operations.keys().collect()
            } else {
                vec![]
            }
        )
    }
}

impl fmt::Debug for Impl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Impl")
         .field("name", &self.name)
         .field("pos", &self.pos)
         .field("patterns", &self.patterns)
         .field("pos", &self.pos)
         .field(
            "operations", 
            &if let Some(operations) = &self.operations {
                operations.keys().collect()
            } else {
                vec![]
            }
        )
         .finish()
    }
}

#[derive(Debug)]
/// Pattern class
pub struct Pattern {
    pub prototype: ast::Namespace,
    pub contents: Vec<Node>,
    pub pos: helpers::Pos
}

#[derive(Debug)]
/// Terminal node in pattern
pub struct Terminal {
    pub contents: ast::StringLiteral,
    pub pos: helpers::Pos
}

#[derive(Debug)]
/// Non-terminal node in pattern
pub struct NonTerminal {
    pub name: ast::DollarID,
    pub prototype: ast::Namespace,
    pub pos: helpers::Pos
}

