mod error;
mod source;
mod span;

pub use error::{Diagnostic, DiagnosticType, Failible, Level};
pub mod prelude {
    pub use super::error::{Diagnostic, DiagnosticType, Diagnostics, Failible, Level};
    pub use super::source::SourceId;
    pub use super::span::Span;
    pub use std::borrow::Cow;
}
pub use source::{SourceId, Sources};
pub use span::Span;
