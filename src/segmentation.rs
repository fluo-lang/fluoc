use crate::sourcemap::SourceMap;

use std::fmt;
use unicode_segmentation::GraphemeCursor;

#[derive(Clone)]
/// Custom version of unicode_segmentation Graphemes
/// ```
/// use crate::sourcemap::SourceMapInner;
/// let sourcemap = SourceMapInner::new();
/// sourcemap.borrow_mut().insert_file();
/// let graphemes = GraphemeIdxs::new(sourcemap, 0);
/// ```
pub struct GraphemeIdxs {
    sourcemap: SourceMap,
    filename_id: usize,
    cursor: GraphemeCursor,
    cursor_back: GraphemeCursor,
}

impl GraphemeIdxs {
    pub fn new(sourcemap: SourceMap, filename_id: usize) -> Self {
        let len = sourcemap.borrow().get_file(filename_id).len();
        GraphemeIdxs {
            sourcemap,
            filename_id,
            cursor: GraphemeCursor::new(0, len, true),
            cursor_back: GraphemeCursor::new(len, len, true),
        }
    }
}

impl Iterator for GraphemeIdxs {
    type Item = Grapheme;

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let slen = self.cursor_back.cur_cursor() - self.cursor.cur_cursor();
        (std::cmp::min(slen, 1), Some(slen))
    }

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let start = self.cursor.cur_cursor();
        if start == self.cursor_back.cur_cursor() {
            return None;
        }
        let next = self
            .cursor
            .next_boundary(self.sourcemap.borrow().get_file(self.filename_id), 0)
            .unwrap()
            .unwrap();
        Some(Grapheme::new(
            &get_file!(self.sourcemap, self.filename_id)[start..next],
            next - start,
        ))
    }
}

#[derive(Clone)]
pub struct Grapheme {
    pub front: char,
    next: Vec<char>,
    len: usize,
}

impl Grapheme {
    /// Create new grapheme
    pub fn new(value: &str, len: usize) -> Self {
        let mut chars = value.chars();
        Grapheme {
            front: chars.next().unwrap(),
            len,
            next: chars.collect(),
        }
    }

    /// Get the length of the grapheme in bytes
    pub fn len(&self) -> usize {
        self.len
    }
}

impl fmt::Display for Grapheme {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.front)?;
        for c in &self.next {
            write!(f, "{}", c)?;
        }
        Ok(())
    }
}
