use smallvec::SmallVec;
use std::fmt::{self, Display};

use crate::diagnostics::Span;

#[derive(Debug, PartialEq, Eq)]
/// An error type
pub enum DiagnosticType {
    UnexpectedCharacter,
}

impl DiagnosticType {
    pub fn description(&self) -> &'static str {
        match self {
            DiagnosticType::UnexpectedCharacter => "unexpected character while lexing",
        }
    }

    pub fn name(&self) -> &'static str {
        match self {
            DiagnosticType::UnexpectedCharacter => "unexpected_character",
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Level {
    Error,
    Warning,
}

#[derive(Debug, PartialEq, Eq)]
struct Annotation {
    level: Level,
    message: String,
    span: Span,
}

#[derive(Debug, PartialEq, Eq)]
/// A generic error
pub struct Error {
    span: Span,
    ty: DiagnosticType,
    level: Level,
    annotations: SmallVec<[Annotation; 1]>,
}

impl Error {
    pub fn build(level: Level, ty: DiagnosticType, span: Span) -> Self {
        Self {
            annotations: SmallVec::new(),
            ty,
            span,
            level,
        }
    }

    pub fn annotation(mut self, level: Level, message: String, span: Span) -> Self {
        self.annotations.push(Annotation {
            level,
            message,
            span,
        });
        self
    }
}

pub type Failible<T> = Result<T, Error>;
