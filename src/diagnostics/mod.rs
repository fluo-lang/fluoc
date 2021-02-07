mod error;
mod source;
mod span;

pub use error::{Diagnostic, DiagnosticType, Failible, Level};
pub mod ErrorPrelude {
    pub use super::error::{Diagnostic, DiagnosticType, Failible, Level};
}
pub use source::{SourceId, Sources};
pub use span::{Span, Spanned};
