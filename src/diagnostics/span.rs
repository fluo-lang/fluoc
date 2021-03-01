use super::SourceId;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
/// A piece of code in a specified file.
pub struct Span(usize, usize, SourceId);

impl Span {
    pub fn new(s: usize, e: usize, source_id: SourceId) -> Self {
        Self(s, e, source_id)
    }

    pub fn dummy() -> Self {
        Self(0, 0, SourceId::dummy())
    }

    pub fn s(&self) -> usize {
        self.0
    }

    pub fn e(&self) -> usize {
        self.1
    }

    pub fn source_id(&self) -> SourceId {
        self.2
    }
}

#[cfg(test)]
mod spanned_tests {
    use super::Span;
    use crate::diagnostics::Sources;

    #[test]
    fn spanned_new() {
        let sid = Sources::new().add_source("test".to_string());
        let spanned = Span::new(0, 0, sid);
        assert_eq!(spanned.0, 0);
        assert_eq!(spanned.1, 0);
        assert_eq!(spanned.2, sid);
    }
}
