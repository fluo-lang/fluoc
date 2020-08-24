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
    fn as_str(&self) -> &'static str {
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

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Style {
    color: Option<Color>,
    font: Option<Font>,
}

impl Style {
    pub fn new(color: Option<Color>, font: Option<Font>) -> Style {
        Style { color, font }
    }
}

#[derive(Clone)]
pub struct Buffer {
    contents: Vec<Vec<char>>,
    styles: Vec<Vec<Style>>,
}

impl Buffer {
    pub fn new() -> Buffer {
        Buffer {
            contents: Vec::new(),
            styles: Vec::new(),
        }
    }

    fn ensure_lines(&mut self, line: usize) {
        while line >= self.contents.len() {
            self.contents.push(vec![]);
            self.styles.push(vec![]);
        }
    }

    pub fn render(&mut self) -> String {
        let mut output = String::new();
        let mut current_style: Option<Style> = None;

        for (row, row_style) in self.contents.iter().zip(&self.styles) {
            for (ch, chs) in row.iter().zip(row_style) {
                if Some(*chs) != current_style {
                    output.push_str(Font::Reset.as_str());
                    current_style = Some(*chs);
                }
                if let Some(color) = current_style.unwrap().color {
                    output.push_str(&color.to_string()[..]);
                }
                if let Some(color) = current_style.unwrap().font {
                    output.push_str(&color.to_string()[..]);
                }
                output.push_str(&ch.to_string()[..]);
            }
            output.push_str("\n");
        }

        output.push_str(Font::Reset.as_str());
        output
    }

    pub fn writech(&mut self, line: usize, col: usize, chr: char, style: Style) -> (usize, usize) {
        self.ensure_lines(line);
        if col < self.contents[line].len() {
            if chr != '\t' {
                self.contents[line][col] = chr;
                self.styles[line][col] = style;
                (line + 1, col)
            } else {
                self.writel(
                    line,
                    self.contents[line].len(),
                    &" ".repeat(4),
                    Style::new(None, None),
                );
                (line + 1, col + 4)
            }
        } else {
            let mut i = self.contents[line].len();
            while i < col {
                self.contents[line].push(' ');
                self.styles[line].push(Style::new(None, None));
                i += 1;
            }
            if chr != '\t' {
                self.contents[line].push(chr);
                self.styles[line].push(style);
                (line + 1, col)
            } else {
                self.writel(
                    line,
                    self.contents[line].len(),
                    &" ".repeat(4),
                    Style::new(None, None),
                );
                (line + 1, col + 4)
            }
        }
    }

    pub fn writechln(
        &mut self,
        line: usize,
        col: usize,
        chr: char,
        style: Style,
    ) -> (usize, usize) {
        self.writech(line, col, chr, style);
        (line + 2, 1)
    }

    pub fn writel(
        &mut self,
        line: usize,
        col: usize,
        string: &str,
        style: Style,
    ) -> (usize, usize) {
        let mut n = col;
        for c in string.chars() {
            self.writech(line, n, c, style);
            if c == '\t' {
                n += 3
            }
            n += 1;
        }
        (line + 1, col + string.len() + 1)
    }

    pub fn writeln(
        &mut self,
        line: usize,
        col: usize,
        string: &str,
        style: Style,
    ) -> (usize, usize) {
        self.writel(line, col, string, style);
        (line + 2, 1)
    }

    pub fn prepend(&mut self, line: usize, string: &str, style: Style) {
        self.ensure_lines(line);
        let string_len = string.chars().count();

        // Push the old content over to make room for new content
        for _ in 0..string_len {
            self.styles[line].insert(0, Style::new(None, None));
            self.contents[line].insert(0, ' ');
        }

        self.writeln(line, 0, string, style);
    }

    pub fn append(&mut self, line: usize, string: &str, style: Style) {
        if line >= self.contents.len() {
            self.writeln(line, 0, string, style);
        } else {
            let col = self.contents[line].len();
            self.writeln(line, col, string, style);
        }
    }

    pub fn reset(&mut self) {
        self.contents.clear();
        self.styles.clear();
    }
}
