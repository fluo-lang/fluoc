use super::interner::StrId;

pub type Failable<T> = Result<T, codespan_reporting::diagnostic::Diagnostic<StrId>>;
