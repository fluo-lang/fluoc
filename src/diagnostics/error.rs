use smallvec::SmallVec;
use std::fmt::{self, Display};

use crate::diagnostics::Span;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
/// An error type
pub enum DiagnosticType {
    UnexpectedCharacter,
    InvalidEscapeSequence,
}

impl DiagnosticType {
    pub fn description(&self) -> &'static str {
        match self {
            DiagnosticType::UnexpectedCharacter => "unexpected character while lexing",
            DiagnosticType::InvalidEscapeSequence => "invalid escaped character",
        }
    }

    pub fn name(&self) -> &'static str {
        match self {
            DiagnosticType::UnexpectedCharacter => "unexpected_character",
            DiagnosticType::InvalidEscapeSequence => "invalid_escape",
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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
pub struct Diagnostic {
    span: Span,
    ty: DiagnosticType,
    level: Level,
    annotations: SmallVec<[Annotation; 1]>,
}

impl Diagnostic {
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

    pub fn ty(&self) -> DiagnosticType {
        self.ty
    }
}

pub type Failible<T> = Result<T, Diagnostic>;

#[cfg(test)]
mod error_tests {
    use super::*;
    use crate::diagnostics::Sources;
    use smallvec::smallvec;

    #[test]
    fn diagnostic_description() {
        assert_eq!(
            DiagnosticType::UnexpectedCharacter.description(),
            DiagnosticType::UnexpectedCharacter.description()
        );
    }

    #[test]
    fn diagnostic_ty() {
        assert_eq!(
            DiagnosticType::UnexpectedCharacter.name(),
            DiagnosticType::UnexpectedCharacter.name()
        );
    }

    #[test]
    fn diagnostic_builder() {
        let sid = Sources::new().add_source("test".to_string());
        let span = Span::new(0, 1, sid);
        let message = "Test message".to_string();
        assert_eq!(
            Diagnostic {
                span,
                level: Level::Warning,
                ty: DiagnosticType::UnexpectedCharacter,
                annotations: smallvec![Annotation {
                    message: message.clone(),
                    level: Level::Error,
                    span
                }],
            },
            Diagnostic::build(Level::Warning, DiagnosticType::UnexpectedCharacter, span)
                .annotation(Level::Error, message, span)
        );
    }
}
