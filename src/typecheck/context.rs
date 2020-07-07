use crate::helpers;
use crate::lexer::TokenType;
use crate::logger::logger::{
    Error, ErrorAnnotation, ErrorDisplayType, ErrorLevel, ErrorOrVec, ErrorType,
};
use crate::parser::ast::*;
use crate::typecheck::ast_typecheck::{ TypeCheckType, TypeCheckTypeType };

use std::collections::HashMap;
use std::fmt;
use std::ops::Deref;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Nullable<'a> {
    Safe,
    Unsafe(ErrorAnnotation<'a>),
    ConditionalSafe(ErrorAnnotation<'a>),
}

impl<'a> PartialEq for Nullable<'a> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Nullable::Safe, Nullable::Safe) => true,
            (Nullable::Unsafe(_), Nullable::Unsafe(_)) => true,
            (Nullable::ConditionalSafe(_), Nullable::ConditionalSafe(_)) => true,
            _ => false,
        }
    }
}

impl<'a> Nullable<'a> {
    pub fn get_error(&self) -> Option<ErrorAnnotation<'a>> {
        match self {
            Nullable::ConditionalSafe(val) => Some(val.clone()),
            Nullable::Safe => None,
            Nullable::Unsafe(val) => Some(val.clone()),
        }
    }

    fn get_safe_cond(&self) -> Option<ErrorAnnotation<'a>> {
        match self {
            Nullable::ConditionalSafe(val) => Some(val.clone()),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum SymbTabObj<'a> {
    CustomType(TypeCheckType<'a>, Visibility),
    Function(TypeCheckType<'a>, helpers::Pos<'a>, bool),
    Variable(TypeCheckType<'a>, Nullable<'a>),
}

impl<'a> SymbTabObj<'a> {
    pub fn unwrap_variable_ref(&self) -> (&TypeCheckType<'a>, &Nullable<'a>) {
        match self {
            SymbTabObj::Variable(expr, safe) => (expr, safe),
            _ => panic!(
                "Tried to unwrap variable from symbtab object, got `{:?}`",
                self
            ),
        }
    }

    pub fn unwrap_type_ref(&self) -> (&TypeCheckType<'a>, Visibility) {
        match self {
            SymbTabObj::CustomType(val, visibility) => (val, *visibility),
            _ => panic!("Tried to unwrap type from symbtab object, got `{:?}`", self),
        }
    }

    pub fn unwrap_function_ref(&self) -> (&TypeCheckType<'a>, helpers::Pos<'a>, bool) {
        match self {
            SymbTabObj::Function(val, pos, mangled) => (val, *pos, *mangled),
            _ => panic!(
                "Tried to unwrap variable from symbtab object, got `{:?}`",
                self
            ),
        }
    }

    pub fn pos(&self) -> helpers::Pos<'a> {
        match &self {
            SymbTabObj::CustomType(val, _) => val.pos,
            SymbTabObj::Function(val, _, _) => val.pos,
            SymbTabObj::Variable(val, _) => val.pos,
        }
    }
}

impl<'a> fmt::Display for SymbTabObj<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let representation = match self {
            SymbTabObj::CustomType(_, _) => "custom type",
            SymbTabObj::Function(_, _, _) => "function",
            SymbTabObj::Variable(_, _) => "variable",
        };
        write!(f, "{}", representation)
    }
}

#[derive(Clone, Debug)]
pub enum CurrentContext<'a> {
    If(helpers::Pos<'a>),
    Else(helpers::Pos<'a>),
    Other,
}
#[derive(Clone, Debug)]
pub struct TypeCheckSymbTab<'a> {
    pub items: HashMap<(Option<TokenType<'a>>, Rc<Namespace<'a>>), Vec<SymbTabObj<'a>>>,
    pub curr_prefix: Vec<NameID<'a>>, // Current Namespace to push
    changed_safe: HashMap<Rc<Namespace<'a>>, Nullable<'a>>,
    pub conditional_state: CurrentContext<'a>,
}

impl<'a> fmt::Display for TypeCheckSymbTab<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut val = String::new();
        for (key, value) in self.items.iter() {
            val += &format!(
                "{}{}: {}\n",
                key.0
                    .map(|value| value.to_string())
                    .unwrap_or("".to_string()),
                key.1,
                value
                    .iter()
                    .map(|val| val.to_string())
                    .collect::<Vec<_>>()
                    .join(",\n")
            )[..];
        }
        write!(f, "{}", val)
    }
}

impl<'a> TypeCheckSymbTab<'a> {
    pub fn new() -> TypeCheckSymbTab<'a> {
        TypeCheckSymbTab {
            items: HashMap::new(),
            curr_prefix: Vec::new(),
            changed_safe: HashMap::new(),
            conditional_state: CurrentContext::Other,
        }
    }

    pub fn set_local(
        &mut self,
        namespace: Rc<Namespace<'a>>,
        value: SymbTabObj<'a>,
    ) -> Rc<Namespace<'a>> {
        self.items
            .entry((None, Rc::clone(&namespace)))
            .or_insert(Vec::new())
            .push(value);
        namespace
    }

    pub fn set_global(
        &mut self,
        namespace: Rc<Namespace<'a>>,
        value: SymbTabObj<'a>,
    ) -> Rc<Namespace<'a>> {
        let adjusted_namespace = Rc::new(
            namespace
                .as_ref()
                .clone()
                .prepend_namespace(&mut self.curr_prefix.clone()),
        );

        self.items
            .entry((None, Rc::clone(&adjusted_namespace)))
            .or_insert(Vec::new())
            .push(value);

        adjusted_namespace
    }

    pub fn set_overloaded(
        &mut self,
        namespace: Rc<Namespace<'a>>,
        overload_val: TokenType<'a>,
        value: SymbTabObj<'a>,
        mangle: bool,
    ) -> Result<Rc<Namespace<'a>>, ErrorOrVec<'a>> {
        let adjusted_namespace = if mangle {
            Rc::new(
                namespace
                    .as_ref()
                    .clone()
                    .prepend_namespace(&mut self.curr_prefix.clone()),
            )
        } else {
            namespace
        };

        self.items
            .entry((Some(overload_val), Rc::clone(&adjusted_namespace)))
            .or_insert(Vec::new())
            .push(value);

        Ok(Rc::clone(&adjusted_namespace))
    }

    pub fn set_function(
        &mut self,
        namespace: Rc<Namespace<'a>>,
        value: SymbTabObj<'a>,
        mangle: bool,
    ) -> Result<Rc<Namespace<'a>>, ErrorOrVec<'a>> {
        let adjusted_namespace = if mangle {
            Rc::new(
                namespace
                    .as_ref()
                    .clone()
                    .prepend_namespace(&mut self.curr_prefix.clone()),
            )
        } else {
            namespace
        };

        self.items
            .entry((None, Rc::clone(&adjusted_namespace)))
            .or_insert(Vec::new())
            .push(value);

        Ok(adjusted_namespace)
    }

    pub fn reset_changed_safe(&mut self) {
        self.changed_safe.clear();
    }

    pub fn insert_changed_safe(&mut self, namespace: Rc<Namespace<'a>>, val: Nullable<'a>) {
        self.changed_safe.insert(namespace, val);
    }

    pub fn get_changed_safe(&self) -> HashMap<Rc<Namespace<'a>>, Nullable<'a>> {
        self.changed_safe.clone()
    }

    pub fn get_safe_value(&self, namespace: Rc<Namespace<'a>>) -> Nullable<'a> {
        if let CurrentContext::If(pos) = self.conditional_state {
            Nullable::ConditionalSafe(ErrorAnnotation::new(
                Some(format!(
                    "consider giving `{}` a value here",
                    namespace.to_string()
                )),
                pos,
                ErrorDisplayType::Info,
            ))
        } else if let CurrentContext::Else(pos) = self.conditional_state {
            Nullable::ConditionalSafe(ErrorAnnotation::new(
                Some(format!(
                    "consider giving `{}` a value here",
                    namespace.to_string()
                )),
                pos,
                ErrorDisplayType::Info,
            ))
        } else {
            Nullable::Safe
        }
    }

    pub fn remove_last(items: &mut Vec<SymbTabObj<'a>>) -> Option<SymbTabObj<'a>> {
        if items.first().is_none() {
            None
        } else {
            Some(items.remove(0))
        }
    }

    pub fn set_unsafe(&mut self, namespace: Rc<Namespace<'a>>, error: ErrorAnnotation<'a>) {
        match Self::remove_last(
            &mut self
                .items
                .remove(&(None, Rc::clone(&namespace)))
                .unwrap_or(Vec::new()),
        ) {
            Some(SymbTabObj::Variable(val, _)) => {
                self.items
                    .entry((None, namespace))
                    .or_insert(Vec::new())
                    .push(SymbTabObj::Variable(val, Nullable::Unsafe(error)));
            }
            _ => {
                panic!("Tried to get item that is not variable");
            }
        }
    }

    pub fn set_safe(&mut self, namespace: Rc<Namespace<'a>>) {
        match Self::remove_last(
            &mut self
                .items
                .remove(&(None, Rc::clone(&namespace)))
                .unwrap_or(Vec::new()),
        ) {
            Some(SymbTabObj::Variable(val, _)) => {
                self.items
                    .entry((None, namespace))
                    .or_insert(Vec::new())
                    .push(SymbTabObj::Variable(val, Nullable::Safe));
            }
            _ => {
                panic!("Tried to get item that is not variable");
            }
        }
    }

    pub fn is_safe_cond(
        symbtab: &HashMap<Rc<Namespace<'a>>, Nullable<'a>>,
        key: Rc<Namespace<'a>>,
    ) -> bool {
        if let Some(val) = symbtab.get(&key) {
            val.get_safe_cond().is_some()
        } else {
            false
        }
    }

    pub fn push_curr_prefix(&mut self, prefix: &mut Vec<NameID<'a>>) {
        self.curr_prefix.append(prefix);
    }

    pub fn get_prefix(
        &self,
        prefix_match: Rc<Namespace<'a>>,
    ) -> HashMap<Rc<Namespace<'a>>, &[SymbTabObj<'a>]> {
        self.items
            .iter()
            .filter(|((_, key), _)| key.starts_with(Rc::clone(&prefix_match)))
            .map(|(key, val)| (Rc::clone(&key.1), &val[..]))
            .collect()
    }

    pub fn get_prefix_op(
        &self,
        real_op: TokenType<'a>,
    ) -> HashMap<Rc<Namespace<'a>>, &[SymbTabObj<'a>]> {
        self.items
            .iter()
            .filter(|((op, _), _)| {
                if let Some(operator) = op {
                    *operator == real_op
                } else {
                    false
                }
            })
            .map(|(key, val)| (Rc::clone(&key.1), &val[..]))
            .collect()
    }

    pub fn get_function(
        &self,
        namespace: Rc<Namespace<'a>>,
    ) -> Result<(Vec<&SymbTabObj<'a>>, Rc<Namespace<'a>>), ErrorOrVec<'a>> {
        let mut possible_error = None;
        match self.items.get(&(None, Rc::clone(&namespace))) {
            Some(val) => {
                if val.len() == 1 {
                    let only_value = val.deref().first().unwrap();
                    match only_value {
                        SymbTabObj::Function(_, _, _) => return Ok((vec![only_value], namespace)),
                        others => {
                            possible_error = Some(ErrorOrVec::Error(
                                Error::new(
                                    format!("`{}` is not a function", namespace),
                                    ErrorType::UndefinedSymbol,
                                    namespace.pos,
                                    ErrorDisplayType::Error,
                                    vec![ErrorAnnotation::new(
                                        Some(format!(
                                            "`{}` is a `{}`, not a function",
                                            namespace, others
                                        )),
                                        namespace.pos,
                                        ErrorDisplayType::Error,
                                    )],
                                    true,
                                ),
                                ErrorLevel::NonExistentType,
                            ));
                        }
                    }
                } else {
                    // More than one value here
                    // TODO: try to infer type

                    let filtered_cands = val
                        .iter()
                        .filter(|value| match value {
                            SymbTabObj::Function(_, _, _) => true,
                            _ => false,
                        })
                        .collect::<Vec<_>>();

                    if !filtered_cands.is_empty() {
                        return Ok((filtered_cands, namespace));
                    }
                }
            }
            None => {}
        }

        // Try other namespace
        let other = Rc::new(
            namespace
                .as_ref()
                .clone()
                .prepend_namespace(&mut self.curr_prefix.clone()),
        );
        match self.items.get(&(None, Rc::clone(&other))) {
            Some(val) => {
                if val.len() == 1 {
                    let only_value = val.deref().first().unwrap();
                    match only_value {
                        SymbTabObj::Function(_, _, _) => {
                            return Ok((vec![only_value], Rc::clone(&other)));
                        }
                        other => {
                            // Cannot get function with and without current prefix
                            possible_error = Some(ErrorOrVec::Error(
                                Error::new(
                                    format!("`{}` is not a function", namespace),
                                    ErrorType::UndefinedSymbol,
                                    namespace.pos,
                                    ErrorDisplayType::Error,
                                    vec![ErrorAnnotation::new(
                                        Some(format!(
                                            "`{}` is a `{}`, not a function",
                                            namespace, other
                                        )),
                                        namespace.pos,
                                        ErrorDisplayType::Error,
                                    )],
                                    true,
                                ),
                                ErrorLevel::NonExistentFunc,
                            ));
                        }
                    }
                } else {
                    let filtered_cands = val
                        .iter()
                        .filter(|value| match value {
                            SymbTabObj::Function(_, _, _) => true,
                            _ => false,
                        })
                        .collect::<Vec<_>>();

                    if !filtered_cands.is_empty() {
                        return Ok((filtered_cands, namespace));
                    }
                }
            }
            None => {}
        }

        if let Some(e) = possible_error {
            return Err(e);
        } else {
            // TODO: add a "did you mean..."
            Err(ErrorOrVec::Error(
                Error::new(
                    "function does not exist".to_string(),
                    ErrorType::UndefinedSymbol,
                    namespace.pos,
                    ErrorDisplayType::Error,
                    vec![ErrorAnnotation::new(
                        Some(format!("undefined function `{}`", namespace)),
                        namespace.pos,
                        ErrorDisplayType::Error,
                    )],
                    true,
                ),
                ErrorLevel::NonExistentFunc,
            ))
        }
    }

    fn generate_private_type_err(
        type_used: Rc<Namespace<'a>>,
        private_type_pos: helpers::Pos<'a>,
    ) -> ErrorOrVec<'a> {
        // Private type and not from the same file
        ErrorOrVec::Error(
            Error::new(
                "use of private type".to_string(),
                ErrorType::VisibilityError,
                type_used.pos,
                ErrorDisplayType::Error,
                vec![
                    ErrorAnnotation::new(
                        Some(format!("type `{}` defined here", type_used.to_string())),
                        private_type_pos,
                        ErrorDisplayType::Info,
                    ),
                    ErrorAnnotation::new(
                        Some("type used here".to_string()),
                        type_used.pos,
                        ErrorDisplayType::Error,
                    ),
                ],
                true,
            ),
            ErrorLevel::TypeError,
        )
    }

    pub fn get_type(
        &self,
        namespace: Rc<Namespace<'a>>,
    ) -> Result<&SymbTabObj<'a>, ErrorOrVec<'a>> {
        let mut possible_error = None;
        match self.items.get(&(None, Rc::clone(&namespace))) {
            Some(val) => match val.deref() {
                [SymbTabObj::CustomType(typechecktype, visibility)] => {
                    return Ok(
                        if *visibility == Visibility::Private
                            && typechecktype.pos.filename != namespace.pos.filename
                        {
                            return Err(Self::generate_private_type_err(
                                Rc::clone(&namespace),
                                typechecktype.pos,
                            ));
                        } else {
                            val.first().unwrap()
                        },
                    );
                }
                other => {
                    possible_error = Some(ErrorOrVec::Error(
                        Error::new(
                            format!("`{}` is not a type", namespace),
                            ErrorType::UndefinedSymbol,
                            namespace.pos,
                            ErrorDisplayType::Error,
                            vec![ErrorAnnotation::new(
                                Some(format!(
                                    "`{}` is a `{}`, not a type",
                                    namespace,
                                    other.first().unwrap()
                                )),
                                namespace.pos,
                                ErrorDisplayType::Error,
                            )],
                            true,
                        ),
                        ErrorLevel::NonExistentType,
                    ));
                }
            },
            None => {}
        }

        // Try other namespace
        let other = Rc::new(
            namespace
                .as_ref()
                .clone()
                .prepend_namespace(&mut self.curr_prefix.clone()),
        );

        match self.items.get(&(None, other)) {
            Some(val) => match val.deref() {
                [SymbTabObj::CustomType(typechecktype, visibility)] => {
                    return Ok(
                        if *visibility == Visibility::Private
                            && typechecktype.pos.filename != namespace.pos.filename
                        {
                            return Err(Self::generate_private_type_err(
                                Rc::clone(&namespace),
                                typechecktype.pos,
                            ));
                        } else {
                            val.first().unwrap()
                        },
                    );
                }
                other => {
                    possible_error = Some(ErrorOrVec::Error(
                        Error::new(
                            format!("`{}` is not a type", namespace),
                            ErrorType::UndefinedSymbol,
                            namespace.pos,
                            ErrorDisplayType::Error,
                            vec![ErrorAnnotation::new(
                                Some(format!(
                                    "`{}` is a `{}`, not a type",
                                    namespace,
                                    other.first().unwrap()
                                )),
                                namespace.pos,
                                ErrorDisplayType::Error,
                            )],
                            true,
                        ),
                        ErrorLevel::NonExistentType,
                    ));
                }
            },
            None => {}
        }

        match possible_error {
            Some(err) => Err(err),
            None => Err(ErrorOrVec::Error(
                Error::new(
                    "type does not exist".to_string(),
                    ErrorType::UndefinedSymbol,
                    namespace.pos,
                    ErrorDisplayType::Error,
                    vec![ErrorAnnotation::new(
                        Some(format!("undefined type `{}`", namespace)),
                        namespace.pos,
                        ErrorDisplayType::Error,
                    )],
                    true,
                ),
                ErrorLevel::NonExistentType,
            )),
        }
    }

    pub fn get_basic_type(
        &self,
        namespace: Rc<Namespace<'a>>,
    ) -> Result<&SymbTabObj<'a>, ErrorOrVec<'a>> {
        let mut type_val = self.get_type(Rc::clone(&namespace))?;
        while let SymbTabObj::CustomType(
            TypeCheckType {
                value: TypeCheckTypeType::CustomType(name, _),
                pos: _,
                inferred: _,
            },
            _,
        ) = type_val
        {
            type_val = self.get_type(Rc::clone(name))?;
        }
        Ok(type_val)
    }

    pub fn get_variable(
        &self,
        namespace: Rc<Namespace<'a>>,
    ) -> Result<&SymbTabObj<'a>, ErrorOrVec<'a>> {
        match self.items.get(&(None, Rc::clone(&namespace))) {
            Some(val) => match val.deref() {
                [SymbTabObj::Variable(_, _)] => return Ok(val.first().unwrap()),
                other => {
                    return Err(ErrorOrVec::Error(
                        Error::new(
                            format!("`{}` is not a variable", namespace),
                            ErrorType::UndefinedSymbol,
                            namespace.pos,
                            ErrorDisplayType::Error,
                            vec![ErrorAnnotation::new(
                                Some(format!(
                                    "`{}` is a `{}`, not a variable",
                                    namespace,
                                    other.last().unwrap()
                                )),
                                namespace.pos,
                                ErrorDisplayType::Error,
                            )],
                            true,
                        ),
                        ErrorLevel::NonExistentVar,
                    ));
                }
            },
            None => {}
        }
        Err(ErrorOrVec::Error(
            Error::new(
                "variable does not exist".to_string(),
                ErrorType::UndefinedSymbol,
                namespace.pos,
                ErrorDisplayType::Error,
                vec![ErrorAnnotation::new(
                    Some(format!("undefined variable `{}`", namespace)),
                    namespace.pos,
                    ErrorDisplayType::Error,
                )],
                true,
            ),
            ErrorLevel::NonExistentVar,
        ))
    }
}
