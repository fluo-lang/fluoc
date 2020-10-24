use crate::parser::ast;

use std::collections::HashMap;
use std::fmt;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

pub struct TOption<T>(Option<T>);

impl<T> Deref for TOption<T> {
    type Target = Option<T>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for TOption<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[derive(Debug, Clone)]
/// Context storing defines types, variables, functions, etc
pub struct Context<T> {
    objects: HashMap<Rc<ast::Namespace>, T>,
}

impl<T: fmt::Display> Context<T> {
    pub fn new() -> Self {
        Context {
            objects: HashMap::new(),
        }
    }

    pub fn get_local(&self, name: &Rc<ast::Namespace>) -> TOption<&T> {
        match self.objects.get(name) {
            Some(val) => TOption(Some(val)),
            _ => TOption(None),
        }
    }

    pub fn set_local(&mut self, name: Rc<ast::Namespace>, value: T) {
        if self.objects.iter().count() == 3 {
            println!("set ({})> {}: {}\n", self.objects.iter().count(), name, value);
        }
        self.objects.insert(name, value);
    }
}

impl<T> fmt::Display for Context<T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.objects
                .iter()
                .map(|(k, v)| format!("> {} = {}", k, v))
                .collect::<Vec<_>>()
                .join("\n")
        )
    }
}
