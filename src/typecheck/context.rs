use super::mir;

use std::collections::HashMap;

enum ContextObj {
    Function(mir::FunctionSig),
    Expr(),
}

/// Context storing defines types, variables, functions, etc
pub struct Context {
    objects: HashMap<mir::Namespace, ContextObj>,
}

impl Context {
    pub fn new() -> Self {
        Context {
            objects: HashMap::new(),
        }
    }
}
