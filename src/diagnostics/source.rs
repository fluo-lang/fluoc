/// A general sourcemap, from `SourceId` to a `&str`.
#[derive(Debug)]
pub struct Sources {
    sources: Vec<String>,
    counter: usize,
}

impl Sources {
    /// Create a new sourcemap
    pub fn new() -> Self {
        Self {
            counter: 0,
            sources: Vec::new(),
        }
    }

    /// Insert a source into the sourcemap and return a unique `SourceId`
    pub fn add_source(&mut self, source: String) -> SourceId {
        let source_id = SourceId(self.counter);
        self.sources.push(source);
        self.counter += 1;
        source_id
    }

    pub fn get_source<'a, 'b>(&'a self, source_id: &'b SourceId) -> Option<&'a str> {
        self.sources.get(source_id.0).map(|s| &s[..])
    }
}

/// A wrapper around `usize` for use with `Sources`
#[derive(Hash, PartialEq, Eq, Debug, Clone, Copy)]
pub struct SourceId(usize);

#[cfg(test)]
mod source_test {
    use super::*;

    #[test]
    fn basic_insert() {
        let mut source = Sources::new();

        let source1 = source.add_source("This is a test source file".to_string());
        let source2 = source.add_source("This is another source file".to_string());

        // The two `SourceId`'s should be difference
        assert_ne!(source1, source2);
    }

    #[test]
    fn basic_get() {
        let mut source = Sources::new();

        let string = "This is a test string".to_string();
        let source1 = source.add_source(string.clone());

        assert_eq!(Some(&string[..]), source.get_source(&source1));
    }

    #[test]
    fn multiple_gets() {
        let mut source = Sources::new();

        let string1 = "This is a test string".to_string();
        let source1 = source.add_source(string1.clone());

        let string2 = "This is another test string".to_string();
        let source2 = source.add_source(string2.clone());

        assert_eq!(Some(&string1[..]), source.get_source(&source1));
        assert_eq!(Some(&string2[..]), source.get_source(&source2));
    }
}
