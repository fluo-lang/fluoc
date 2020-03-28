
#[derive(Copy, Clone)]
pub enum Color {
    BLACK,
    RED,
    GREEN,
    YELLOW,
    BLUE,
    MAGENTA,
    CYAN,
    WHITE
}

pub mod color {
    pub const BLACK: &'static str = "\x1b[30m";
    pub const BOLD: &'static str= "\x1b[1m";
    pub const RED: &'static str = "\x1b[31m";
    pub const GREEN: &'static str = "\x1b[32m";
    pub const YELLOW: &'static str = "\x1b[33m";
    pub const BLUE: &'static str = "\x1b[34m";
    pub const MAGENTA: &'static str = "\x1b[35m";
    pub const CYAN: &'static str = "\x1b[36m";
    pub const WHITE: &'static str = "\x1b[37m";
    pub const UNDERLINE: &'static str = "\x1b[4m";
    pub const RESET: &'static str = "\x1b[0m";
}

impl Color {
    pub fn to_string(&self) -> &'static str {
        match self {
            Color::BLACK => color::BLACK,
            Color::RED => color::RED,
            Color::GREEN => color::GREEN,
            Color::YELLOW => color::YELLOW,
            Color::BLUE => color::BLUE,
            Color::MAGENTA => color::MAGENTA,
            Color::CYAN => color::CYAN,
            Color::WHITE => color::WHITE,
        }
    }
}

#[derive(Copy, Clone)]
pub enum Font {
    RESET,
    BOLD
}

impl Font {
    pub fn to_string(&self) -> &'static str {
        match self {
            Font::RESET => color::RESET,
            Font::BOLD => color::BOLD
        }
    }
}

#[derive(Copy, Clone)]
pub struct Style {
    color: Option<Color>,
    font: Option<Font>
}

impl Style {
    pub fn new(color: Option<Color>, font: Option<Font>) -> Style {
        Style { color, font }
    }
}

#[derive(Clone)]
pub struct Buffer {
    contents: Vec<Vec<char>>,
    styles: Vec<Vec<Style>>
}

impl Buffer {
    pub fn new() -> Buffer {
        Buffer {
            contents: Vec::new(),
            styles: Vec::new()
        }
    }

    fn ensure_lines(&mut self, line: usize) {
        while line >= self.contents.len() {
            self.contents.push(vec![]);
            self.styles.push(vec![]);
        }
    }

    pub fn render(&mut self) {
        for (row, row_style) in self.contents.iter().zip(&self.styles) {
            println!("{:?}", row);
        }
    }

    pub fn writech(&mut self, line: usize, col: usize, chr: char, style: Style) -> (usize, usize) {
        self.ensure_lines(line);
        if col < self.contents[line].len() {
            self.contents[line][col] = chr;
            self.styles[line][col] = style;
        } else {
            let mut i = self.contents[line].len();
            while i < col {
                self.contents[line].push(' ');
                self.styles[line].push(Style::new(None, None));
                i += 1;
            }
            self.contents[line].push(chr);
            self.styles[line].push(style);
        }
        (line+1, col)
    }

    pub fn writechln(&mut self, line: usize, col: usize, chr: char, style: Style) -> (usize, usize) {
        self.writech(line, col, chr, style);
        (line+2, 1)
    }

    pub fn writel(&mut self, line: usize, col: usize, string: &str, style: Style) -> (usize, usize) {
        let mut n = col;
        for c in string.chars() {
            self.writech(line, n, c, style);
            n += 1;
        }
        (line+1, col+string.len()+1)
    }

    pub fn writeln(&mut self, line: usize, col: usize, string: &str, style: Style) -> (usize, usize) {
        self.writel(line, col, string, style);
        (line+2, 1)
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
}
