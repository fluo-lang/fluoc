mod error;
mod source;
mod span;

pub use error::{Diagnostic, DiagnosticType, Failible, Level};
pub mod DiagnosticsPrelude {
    pub use super::error::{Diagnostic, DiagnosticType, Failible, Level};
}
pub use source::{SourceId, Sources};
pub use span::Span;
