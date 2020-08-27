use super::mir;
use super::types;

use crate::parser::ast;

use std::collections::HashMap;

enum ContextObj {
    Function(types::FunctionSig),
    Expr(mir::MirExpr),
}

/// Context storing defines types, variables, functions, etc
pub struct Context {
    objects: HashMap<ast::Namespace, ContextObj>,
}

impl Context {
    pub fn new() -> Self {
        Context {
            objects: HashMap::new(),
        }
    }
}
