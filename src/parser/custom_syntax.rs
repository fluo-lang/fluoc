use std::collections::HashMap;
use crate::parser::ast::{ NameID, Node };
use crate::logger::logger::Error;
use crate::parser::parser::Parser;

pub enum NamespaceObj {
    ParserNamespace { objects: HashMap<NameID, NamespaceObj> },
    ParserRule(fn (&mut Parser) -> Result<Node, Error>)
}
