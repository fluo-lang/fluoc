mod error;
mod source;
mod span;

pub use error::{DiagnosticType, Error, Failible, Level};
pub mod ErrorPrelude {
    pub use super::error::{DiagnosticType, Error, Failible, Level};
}
pub use source::{SourceId, Sources};
pub use span::{Span, Spanned};
