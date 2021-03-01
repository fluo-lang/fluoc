use smallvec::{smallvec, SmallVec};
use std::borrow::Cow;

use crate::diagnostics::Span;

pub type Failible<T> = Result<T, Diagnostics>;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Diagnostics(SmallVec<[Diagnostic; 1]>);

impl Diagnostics {
    pub fn extend(&mut self, other: &mut Self) {
        self.0.append(&mut other.0);
    }

    pub fn push(&mut self, other: Diagnostic) {
        self.0.push(other);
    }

    pub fn inner(&self) -> &SmallVec<[Diagnostic; 1]> {
        &self.0
    }

    pub fn into_inner(self) -> SmallVec<[Diagnostic; 1]> {
        self.0
    }
}

impl From<Diagnostic> for Diagnostics {
    fn from(diagnostic: Diagnostic) -> Self {
        Self(smallvec![diagnostic])
    }
}

impl From<SmallVec<[Diagnostic; 1]>> for Diagnostics {
    fn from(diagnostics: SmallVec<[Diagnostic; 1]>) -> Self {
        Self(diagnostics)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
/// An error type
pub enum DiagnosticType {
    UnexpectedCharacter = 1,
    InvalidEscapeSequence = 2,
    Syntax = 3,
}

impl DiagnosticType {
    pub fn description(&self) -> &'static str {
        match self {
            DiagnosticType::UnexpectedCharacter => "unexpected character while lexing",
            DiagnosticType::InvalidEscapeSequence => "invalid escaped character",
            DiagnosticType::Syntax => "syntax error",
        }
    }

    pub fn name(&self) -> &'static str {
        match self {
            DiagnosticType::UnexpectedCharacter => "unexpected_character",
            DiagnosticType::InvalidEscapeSequence => "invalid_escape",
            DiagnosticType::Syntax => "syntax",
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Level {
    Error,
    Warning,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Annotation {
    pub level: Level,
    pub message: Cow<'static, str>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Clone)]
/// A generic error
pub struct Diagnostic {
    pub span: Span,
    pub ty: DiagnosticType,
    pub level: Level,
    pub annotations: SmallVec<[Annotation; 1]>,
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

    pub fn into_diagnostics(self) -> Diagnostics {
        Diagnostics(smallvec![self])
    }

    pub fn annotation(mut self, level: Level, message: Cow<'static, str>, span: Span) -> Self {
        self.annotations.push(Annotation {
            level,
            message,
            span,
        });
        self
    }
}

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
        let message: Cow<'static, str> = Cow::Owned("Test message".to_string());
        assert_eq!(
            Diagnostic {
                span,
                level: Level::Warning,
                ty: DiagnosticType::UnexpectedCharacter,
                annotations: smallvec![Annotation {
                    message: message.to_owned(),
                    level: Level::Error,
                    span
                }],
            },
            Diagnostic::build(Level::Warning, DiagnosticType::UnexpectedCharacter, span)
                .annotation(Level::Error, message, span)
        );
    }

    #[test]
    fn diagnostic_into() {
        let sid = Sources::new().add_source("test".to_string());
        let span = Span::new(0, 1, sid);
        let message = Cow::Owned("Test message".to_string());

        let diagnostic =
            Diagnostic::build(Level::Warning, DiagnosticType::UnexpectedCharacter, span)
                .annotation(Level::Error, message, span);

        let diagnostics = Diagnostics(smallvec![diagnostic.clone()]);

        assert_eq!(diagnostics, diagnostic.clone().into());
        let expected_diagnostics: SmallVec<[Diagnostic; 1]> = smallvec![diagnostic.clone()];
        assert_eq!(diagnostics.inner(), &expected_diagnostics);
        assert_eq!(diagnostics.into_inner(), expected_diagnostics);
    }
}
