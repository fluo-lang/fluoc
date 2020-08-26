use std::fmt;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Color {
    Black,
    Red,
    Green,
    Yellow,
    Blue,
    Magenta,
    Cyan,
    White,
    Gray,
    LinenoColor,
}

impl Color {
    pub fn as_str(&self) -> &'static str {
        match self {
            Color::Black => "\x1b[30m",
            Color::Red => "\x1b[31m",
            Color::Green => "\x1b[32m",
            Color::Yellow => "\x1b[33m",
            Color::Blue => "\x1b[34m",
            Color::Magenta => "\x1b[35m",
            Color::Cyan => "\x1b[36m",
            Color::White => "\x1b[37m",
            Color::Gray => "\x1b[30;1m",
            Color::LinenoColor => "\x1b[34m",
        }
    }
}

impl fmt::Display for Color {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Font {
    Reset,
    Bold,
    Underline,
}

impl Font {
    pub fn as_str(&self) -> &'static str {
        match self {
            Font::Reset => "\x1b[0m",
            Font::Bold => "\x1b[1m",
            Font::Underline => "\x1b[4m",
        }
    }
}

impl fmt::Display for Font {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}
