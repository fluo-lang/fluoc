use std::{
    borrow::Cow,
    fmt::{Debug, Display},
    ops::Range,
};

use codespan_reporting::diagnostic::{Diagnostic as CSDiagnostic, Label, LabelStyle, Severity};

#[derive(Debug, Default, Clone, Copy)]
pub struct FileId(usize);

#[derive(Debug, Clone, Copy)]
pub struct Span {
    s: usize,
    e: usize,
    fid: FileId,
}

#[derive(Debug)]
pub struct Tagged<V: Debug, T: Debug>(pub V, pub T);
pub type Spanned<T> = Tagged<T, Span>;

pub type Failable<T> = Result<T, Diagnostic>;

impl Span {
    pub fn new(s: usize, e: usize, fid: FileId) -> Self {
        Self { s, e, fid }
    }

    pub fn single_char(s: usize, fid: FileId) -> Self {
        Self { s, e: s + 1, fid }
    }
}

impl Into<Range<usize>> for Span {
    fn into(self) -> Range<usize> {
        self.s..self.e
    }
}

#[derive(Debug)]
pub struct Diagnostic {
    ty: ErrorType,
    msg: Cow<'static, str>,
    span: Span,
    annotations: Vec<Annotation>,
}

impl Diagnostic {
    pub fn new(ty: ErrorType, msg: &'static str, span: Span) -> Self {
        Self {
            annotations: Vec::new(),
            ty,
            msg: Cow::Borrowed(msg),
            span,
        }
    }

    pub fn new_owned(ty: ErrorType, msg: String, span: Span) -> Self {
        Self {
            annotations: Vec::new(),
            ty,
            msg: Cow::Owned(msg),
            span,
        }
    }

    pub fn with_annot_owned(mut self, msg: String, span: Span) -> Self {
        self.annotations.push(Annotation::new_owned(msg, span));
        self
    }

    pub fn with_annot(mut self, msg: &'static str, span: Span) -> Self {
        self.annotations.push(Annotation::new(msg, span));
        self
    }

    pub fn with_annot_no_msg(mut self, span: Span) -> Self {
        self.annotations.push(Annotation::new_msgless(span));
        self
    }
}

impl Into<CSDiagnostic<FileId>> for Diagnostic {
    fn into(self) -> CSDiagnostic<FileId> {
        CSDiagnostic::new(Severity::Error)
            .with_code(self.ty)
            .with_message(self.msg)
            .with_labels(self.annotations.into_iter().map(|a| a.into()).collect())
    }
}

#[derive(Debug)]
pub struct Annotation {
    msg: Option<Cow<'static, str>>,
    span: Span,
}

impl Into<Label<FileId>> for Annotation {
    fn into(self) -> Label<FileId> {
        let mut label = Label::new(LabelStyle::Primary, self.span.fid, self.span);
        if let Some(msg) = self.msg {
            label = label.with_message(msg);
        }
        label
    }
}

impl Annotation {
    pub fn new_owned(msg: String, span: Span) -> Self {
        Self {
            msg: Some(Cow::Owned(msg)),
            span,
        }
    }

    pub fn new_msgless(span: Span) -> Self {
        Self { msg: None, span }
    }

    pub fn new(msg: &'static str, span: Span) -> Self {
        Self {
            msg: Some(Cow::Borrowed(msg)),
            span,
        }
    }
}

#[derive(Debug)]
pub enum ErrorType {
    LexError,
}

impl Display for ErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::LexError => "lex_error",
            }
        )
    }
}

impl Into<String> for ErrorType {
    fn into(self) -> String {
        self.to_string()
    }
}
