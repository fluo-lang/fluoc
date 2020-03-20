use std::fs;
use std::io;

pub fn read_file(filename: &str) -> io::Result<fs::File> {
    let f = fs::File::open(filename)?;
    Ok(f)
}

pub struct Pos {
    s: i64,
    e: i64
}
