use crate::parser::parser::Parser;
use crate::parser::custom_syntax::NamespaceObj;
use crate::parser::ast::NameID;
use crate::helpers::Pos;
use std::collections::HashMap;

macro_rules! map (
    { $($key:expr => $value:expr),+ } => {
        {
            let mut m = HashMap::new();
            $(
                m.insert($key, $value);
            )+
            m
        }
     };
);

fn new_name_id(name: &str) -> NameID {
    NameID {
        value: name.to_string(), 
        pos: Pos {s: 0, e: 0}
    }
}

fn generate_statement_lib() -> NamespaceObj {
    let objects = map![
        new_name_id("parse") => NamespaceObj::ParserRule(Parser::std_statement_parse),
        new_name_id("run") => NamespaceObj::ParserRule(Parser::std_statement_run)
    ];

    NamespaceObj::ParserNamespace {
        objects: objects
    }
}

pub fn generate_std_lib() -> NamespaceObj {
    let objects = map![
        new_name_id("statement") => generate_statement_lib()
    ];

    NamespaceObj::ParserNamespace {
        objects: objects
    }
}
