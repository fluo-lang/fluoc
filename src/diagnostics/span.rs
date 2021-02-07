use std::ops::{Deref, DerefMut};

use super::SourceId;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
/// A piece of code in a specified file.
pub struct Span(usize, usize, SourceId);

impl Span {
    pub fn new(s: usize, e: usize, source_id: SourceId) -> Self {
        Self(s, e, source_id)
    }

    pub fn spanned<T>(self, inner: T) -> Spanned<T> {
        Spanned(inner, self)
    }
}

/// A spanned object
#[derive(Debug, PartialEq, Eq)]
pub struct Spanned<T>(T, Span);

impl<T> Deref for Spanned<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[cfg(test)]
mod spanned_tests {
    use super::{DerefMut, Span};
    use crate::diagnostics::Sources;

    #[test]
    fn spanned_deref() {
        let sid = Sources::new().add_source("test".to_string());
        let spanned = Span::new(0, 0, sid).spanned(10);
        assert_eq!(*spanned, 10);
    }

    #[test]
    fn spanned_deref_mut() {
        let sid = Sources::new().add_source("test".to_string());
        let mut spanned = Span::new(0, 0, sid).spanned(String::new());
        DerefMut::deref_mut(&mut spanned).push('a');
        assert_eq!(&(*spanned)[..], "a");
    }
}
