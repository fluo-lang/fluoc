use crate::helpers;

use std::cell::RefCell;
use std::collections::HashMap;
use std::path;
use std::rc::Rc;

/// Convenience type for general usage
pub type SourceMap = Rc<RefCell<SourceMapInner>>;

#[derive(Debug, Clone)]
/// A mapping of every file in a project
pub struct SourceMapInner {
    files: HashMap<usize, String>,
    file_ids: HashMap<usize, path::PathBuf>,
    inserted_values: HashMap<usize, String>,
    current_id: usize,
    current_id_inserted: usize,
}

impl SourceMapInner {
    pub fn new() -> SourceMap {
        Rc::new(RefCell::new(SourceMapInner {
            files: HashMap::new(),
            file_ids: HashMap::new(),
            inserted_values: HashMap::new(),
            current_id: 0,
            current_id_inserted: 0,
        }))
    }

    pub fn insert_file(&mut self, filename: path::PathBuf, contents: String) -> usize {
        self.file_ids.insert(self.current_id, filename);
        self.files.insert(self.current_id, contents);
        let original = self.current_id;
        self.current_id += 1;
        original
    }

    pub fn insert_string(&mut self, contents: String, mut span: helpers::Span) -> helpers::Span {
        self.current_id_inserted += 1;
        self.inserted_values
            .insert(self.current_id_inserted, contents);
        span.filename_id = self.current_id_inserted;
        span.is_inserted = true;
        span
    }

    #[inline]
    pub fn get_segment(&self, pos: helpers::Span) -> &str {
        if !pos.is_inserted {
            &self.files[&pos.filename_id][pos.s..pos.e]
        } else {
            &self.inserted_values[&pos.filename_id]
        }
    }

    #[inline]
    pub fn get_filename(&self, idx: usize) -> &path::Path {
        &self.file_ids[&idx]
    }

    #[inline]
    pub fn get_file(&self, idx: usize) -> &str {
        &self.files[&idx]
    }

    pub fn get_files(&self) -> &HashMap<usize, String> {
        &self.files
    }

    pub fn get_ids(&self) -> &HashMap<usize, path::PathBuf> {
        &self.file_ids
    }
}

macro_rules! get_filename {
    ($sourcemap: expr, $idx: expr) => {
        $sourcemap.borrow().get_filename($idx)
    };
}

macro_rules! get_file {
    ($sourcemap: expr, $idx: expr) => {
        $sourcemap.borrow().get_file($idx)
    };
}

macro_rules! get_segment {
    ($sourcemap: expr, $pos: expr) => {
        $sourcemap.borrow().get_segment($pos)
    };
}

macro_rules! insert_file {
    ($sourcemap: expr, $filename: expr, $contents: expr) => {
        $sourcemap.borrow_mut().insert_file($filename, $contents)
    };
}
