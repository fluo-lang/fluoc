use crate::helpers::Pos;
use crate::logger::buffer_writer::{color, Buffer, Color, Font, Style};
use std::cmp::{max, Reverse};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone)]
/// An error type, i.e `Syntax` error or `UnexpectedToken` error
pub enum ErrorType {
    Syntax,
    UnexpectedToken,
    UnterminatedString,
    UnknownCharacter,
    UndefinedSyntax,
    SyntaxTypeError,
}

impl ErrorType {
    fn as_str(&self) -> String {
        match *self {
            ErrorType::Syntax => String::from("syntax_error"),
            ErrorType::UnexpectedToken => String::from("unexpected_token"),
            ErrorType::UnterminatedString => String::from("unterminated_string"),
            ErrorType::UnknownCharacter => String::from("unknown_character"),
            ErrorType::UndefinedSyntax => String::from("undefined_syntax"),
            ErrorType::SyntaxTypeError => String::from("syntax_type_error"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
/// Error display mode
pub enum ErrorDisplayType {
    /// "Error" mode, make underline red and text red
    Error,
    /// "Warning" mode, make underline yellow and text yellow
    Warning,
    /// "Info" mode, make underline Blue and text blue
    Info,
}

impl ErrorDisplayType {
    fn plural(&self) -> &str {
        match self {
            ErrorDisplayType::Error => "Errors",
            ErrorDisplayType::Warning => "Warnings",
            _ => "Errors",
        }
    }

    fn singular(&self) -> &str {
        match self {
            ErrorDisplayType::Error => "Error",
            ErrorDisplayType::Warning => "Warning",
            _ => "Error",
        }
    }

    fn get_underline(&self) -> &str {
        match self {
            ErrorDisplayType::Error => "^",
            ErrorDisplayType::Warning => "~",
            ErrorDisplayType::Info => "-",
        }
    }

    fn get_color(self) -> &'static str {
        match self {
            ErrorDisplayType::Error => color::RED,
            ErrorDisplayType::Warning => color::YELLOW,
            ErrorDisplayType::Info => color::BLUE,
        }
    }

    fn get_color_class(self) -> Color {
        match self {
            ErrorDisplayType::Error => Color::RED,
            ErrorDisplayType::Warning => Color::YELLOW,
            ErrorDisplayType::Info => Color::BLUE,
        }
    }
}

#[derive(Debug, Clone)]
/// Underlines and such
pub struct ErrorAnnotation<'a> {
    /// Error message
    message: Option<String>,
    /// Error position
    position: Pos,
    /// Error display mode
    mode: ErrorDisplayType,
    /// Filename of annotation
    filename: &'a str,
    /// Position
    position_rel: ((usize, usize), (usize, usize)),
}

impl<'a> ErrorAnnotation<'a> {
    /// Returns an error annotation
    ///
    /// Arguments
    ///
    /// * `message`: error message
    /// * `position`: position of error
    /// * `mode`: mode of error report
    /// * `filename`: filename of annotation
    pub fn new(
        message: Option<String>,
        position: Pos,
        mode: ErrorDisplayType,
        filename: &'a str,
    ) -> ErrorAnnotation {
        ErrorAnnotation {
            message,
            position,
            mode,
            filename,
            position_rel: ((0, 0), (0, 0)),
        }
    }

    pub fn has_label(&self) -> bool {
        self.message.is_some()
    }
}

#[derive(Debug, Clone)]
/// An full on error containing useful info.
pub struct Error<'a> {
    /// Error message
    message: String,
    /// Error type
    error: ErrorType,
    /// Error position
    position: Pos,
    /// Error display mode
    mode: ErrorDisplayType,
    /// Filename of error
    filename: &'a str,
    /// Annotations
    annotations: Vec<ErrorAnnotation<'a>>,
    /// Urgent error: raise even if another function parses further
    pub urgent: bool,
}

impl Error<'_> {
    /// Returns an error object
    ///
    /// # Arguments
    ///
    /// * `message`: error message
    /// * `error`: error type
    /// * `position`: position of error
    /// * `token`: optional token associated with error
    /// * `mode`: mode of error report
    pub fn new<'a>(
        message: String,
        error: ErrorType,
        position: Pos,
        mode: ErrorDisplayType,
        filename: &'a str,
        annotations: Vec<ErrorAnnotation<'a>>,
        urgent: bool,
    ) -> Error<'a> {
        Error {
            message,
            error,
            position,
            mode,
            filename,
            annotations,
            urgent,
        }
    }

    pub fn is_priority(&self) -> bool {
        self.urgent
    }
}

#[derive(Clone)]
/// Logger object for one file
pub struct Logger<'a> {
    /// Vector of errors
    errors: Vec<Error<'a>>,
    /// Filename + file contents the logger serves
    filename_contents: HashMap<&'a str, &'a str>,
    /// Amount of indentation used for error
    indentation: String,
    /// Buffer object
    buffer: Buffer,
}

impl<'a> Logger<'a> {
    pub fn new() -> Logger<'a> {
        Logger {
            errors: Vec::new(),
            filename_contents: HashMap::new(),
            indentation: "  ".to_string(),
            buffer: Buffer::new(),
        }
    }

    /// Adds a file to the logger hash map
    ///
    /// Arguments
    ///
    /// * `filename`: filename of the file
    /// * `file_contents`: contents of the file
    pub fn add_file(&mut self, filename: &'a str, file_contents: &'a str) {
        self.filename_contents.insert(filename, file_contents);
    }

    /// Pushes an error onto the error vector.
    pub fn error(&mut self, error: Error<'a>) {
        self.errors.push(error);
    }

    fn get_lineno(&mut self, pos: usize, filename: &'a str) -> (usize, usize) {
        let mut lineno = 1;
        let mut relative_pos = 1;

        for c in (&self.filename_contents[filename][..pos]).chars() {
            relative_pos += 1;
            if c == '\n' {
                lineno += 1;
                relative_pos = 1;
            }
        }

        (lineno, relative_pos)
    }

    fn get_max_line_size(&mut self, errors: &[ErrorAnnotation]) -> usize {
        let mut max_line_size = 0;
        for error in errors {
            let temp = (error.position_rel.1)
                .0
                .to_string()
                .as_str()
                .chars()
                .count();
            if temp > max_line_size {
                max_line_size = temp;
            }
        }
        max_line_size
    }

    fn add_pipe(
        &mut self,
        ln: usize,
        max_line_size: usize,
        vertical_annotations: &HashMap<usize, ErrorAnnotation>,
        lineno: usize,
        end: bool,
    ) -> (usize, usize) {
        // Add vertical pipe:
        //
        //    |
        //
        let temp_pos = self.buffer.writel(
            ln,
            max_line_size + self.indentation.len() * 2,
            "|",
            Style::new(Some(Color::BLUE), Some(Font::BOLD)),
        );

        let mut vertical_annotations: Vec<(&usize, &ErrorAnnotation)> =
            vertical_annotations.iter().collect();
        vertical_annotations.sort_by_key(|a| Reverse(a.0));
        let mut span_no = 0;

        for (ann_ln, annotation) in vertical_annotations {
            if if end { ann_ln + 1 } else { *ann_ln } > lineno
                && lineno > (annotation.position_rel.0).0
            {
                self.buffer.writel(
                    ln,
                    temp_pos.1 + span_no,
                    "|",
                    Style::new(Some(annotation.mode.get_color_class()), Some(Font::BOLD)),
                );
                span_no += 1;
            }
        }
        temp_pos
    }

    fn add_pipe_pure(&mut self, ln: usize, max_line_size: usize) -> (usize, usize) {
        self.buffer.writel(
            ln,
            max_line_size + self.indentation.len() * 2,
            "|",
            Style::new(Some(Color::BLUE), Some(Font::BOLD)),
        )
    }

    fn insert_lineno(&mut self, ln: usize, max_line_size: usize, line_no: usize) {
        self.buffer.writel(
            ln - 1,
            self.indentation.len() + (max_line_size + 1 - format!("{}", line_no).len()),
            &format!("{}", line_no),
            Style::new(Some(Color::BLUE), Some(Font::BOLD)),
        );
    }

    fn get_line_string(&mut self, ln: usize, filename: &'a str) -> String {
        self.filename_contents[filename]
            .split('\n')
            .nth(ln - 1)
            .unwrap()
            .to_string()
    }

    fn num_overlap(
        a_start: usize,
        a_end: usize,
        b_start: usize,
        b_end: usize,
        inclusive: bool,
    ) -> bool {
        let extra = if inclusive { 1 } else { 0 };
        (b_start..b_end + extra).contains(&a_start) || (a_start..a_end + extra).contains(&b_start)
    }

    fn overlaps(a1: &ErrorAnnotation, a2: &ErrorAnnotation, padding: usize) -> bool {
        Logger::num_overlap(
            a1.position.s,
            a1.position.e + padding,
            a2.position.s,
            a2.position.e,
            false,
        )
    }

    fn draw_last_multiline(
        &mut self,
        writer_pos: &mut (usize, usize),
        annotation: &ErrorAnnotation,
        max_line_size: usize,
        line_offset: &mut usize,
        vertical_annotations: &HashMap<usize, ErrorAnnotation>,
        lineno: usize,
        span_width: usize,
        span_number: usize,
    ) {
        // Last line of annotation
        let repeat = span_width + span_number - 1;
        *writer_pos = self.buffer.writel(
            writer_pos.0 - 1,
            max_line_size + self.indentation.len() + 3 + span_width - span_number,
            &format!("|{}", "_".repeat(if repeat == 0 { 1 } else { repeat - 1 })),
            Style::new(Some(annotation.mode.get_color_class()), Some(Font::BOLD)),
        );

        *writer_pos = self.buffer.writel(
            writer_pos.0 - 1,
            writer_pos.1 - 1,
            &annotation
                .mode
                .get_underline()
                .repeat((annotation.position_rel.1).1 - 1),
            Style::new(Some(annotation.mode.get_color_class()), Some(Font::BOLD)),
        );

        *writer_pos = self.buffer.writel(
            writer_pos.0 - 1,
            writer_pos.1,
            &annotation.message.as_ref().unwrap_or(&"".to_string()),
            Style::new(Some(annotation.mode.get_color_class()), Some(Font::BOLD)),
        );

        writer_pos.0 += 1;
        self.add_pipe(
            writer_pos.0 - 1,
            max_line_size,
            vertical_annotations,
            lineno,
            false,
        );
        *line_offset += 1;
    }

    fn draw_line(
        &mut self,
        writer_pos: &mut (usize, usize),
        annotation: &&ErrorAnnotation<'a>,
        max_line_size: usize,
        span_thickness: usize,
        lineno: usize,
        prev_line: &mut usize,
        printed_lines: &mut HashMap<&'a str, HashSet<usize>>,
        first: bool,
        line_offset: &mut usize,
        vertical_annotations: &HashMap<usize, ErrorAnnotation<'a>>,
    ) {
        // Add pipe on the left
        *writer_pos = self.add_pipe(
            writer_pos.0 - 1,
            max_line_size,
            &vertical_annotations,
            lineno,
            true,
        );

        writer_pos.1 += 1; // add two proceeding spaces
        let line = self.get_line_string(lineno, annotation.filename); // get line of code

        // Add line of code to buffer
        *writer_pos = self.buffer.writel(
            writer_pos.0 - 1,
            writer_pos.1 + span_thickness,
            &line,
            Style::new(None, None),
        );
        self.add_pipe(
            writer_pos.0 - 1,
            max_line_size,
            &vertical_annotations,
            lineno,
            false,
        );
        self.insert_lineno(writer_pos.0, max_line_size, lineno); // insert line number on the left of pipe

        // Add dots if we are not displaying lines continually
        if !first && lineno - *prev_line > 1 {
            self.buffer.writel(
                writer_pos.0 - 2,
                max_line_size + self.indentation.len() - 1,
                "...",
                Style::new(Some(Color::BLUE), None),
            );
            self.buffer.writech(
                writer_pos.0 - 2,
                max_line_size + self.indentation.len() + 2,
                ' ',
                Style::new(Some(Color::BLUE), None),
            );
        }

        *line_offset += 1;

        // remember we did this, so we don't need to do it again
        if printed_lines.contains_key(annotation.filename) {
            printed_lines
                .get_mut(annotation.filename)
                .unwrap()
                .insert(lineno);
        } else {
            let mut lines: HashSet<usize> = HashSet::new();
            lines.insert(lineno);
            printed_lines.insert(annotation.filename, lines);
        }

        // Store previous line
        *prev_line = lineno;
    }

    fn format_line(
        &mut self,
        mut annotations: Vec<ErrorAnnotation<'a>>,
        writer_pos: &mut (usize, usize),
        span_thickness: usize,
        max_line_size: usize,
    ) {
        let mut printed_lines: HashMap<&str, HashSet<usize>> = HashMap::new();
        let start_line = writer_pos.0 - 1;
        let mut annotations_by_line: Vec<Vec<ErrorAnnotation>> = Vec::new();
        let mut temp: Vec<ErrorAnnotation> = Vec::new();
        let mut first = true;

        annotations.sort_by_key(|x| (x.position_rel.0).0);

        for annotation in annotations {
            if !first {
                if (temp[temp.len() - 1].position_rel.0).0 != (annotation.position_rel.0).0 {
                    annotations_by_line.push(temp.clone());
                    temp.clear();
                }
            } else {
                first = false;
            }
            temp.push(annotation.clone());
        }

        annotations_by_line.push(temp.clone());
        temp.clear();
        let mut line_offset: usize = 0;
        let mut prev_line: usize = 0;
        let mut prev_line_2: usize = 0;
        let mut first = true;
        let mut vertical_annotations: HashMap<usize, ErrorAnnotation> = HashMap::new(); // for last part of multi-line block annotation
        let mut span_no = 0;

        for mut annotations in annotations_by_line {
            annotations.sort_by_key(|v| (Reverse(v.position.s), Reverse(v.position.e)));

            let mut annotation_pos = Vec::new();

            // We want to sort annotations
            let mut p = 0;
            let mut line_len = 0;
            for (i, annotation) in annotations.iter().enumerate() {
                for (j, next) in annotations.iter().enumerate() {
                    if Logger::overlaps(next, annotation, 0)
                        && annotation.has_label()
                        && j > i
                        && p == 0
                    {
                        if next.position.s == annotation.position.s
                            && next.position.e == annotation.position.e
                            && !next.has_label()
                        {
                            continue;
                        }

                        // This annotation needs a new line in the output.
                        p += 1;
                        break;
                    }
                }

                annotation_pos.push((p, annotation));
                for (j, next) in annotations.iter().enumerate() {
                    if j > i {
                        let l = next.message.as_ref().map_or(0, |label| label.len() + 2);
                        if Logger::overlaps(next, annotation, l)
                            && annotation.has_label()
                            && next.has_label()
                            || (self.is_multiline(annotation) && next.has_label())
                            || (annotation.has_label() && self.is_multiline(next))
                            || (self.is_multiline(annotation) && self.is_multiline(next))
                            || (Logger::overlaps(next, annotation, l)
                                && next.position.e <= annotation.position.e
                                && next.has_label()
                                && p == 0)
                        {
                            p += 1;
                            break;
                        }
                    }
                }
                line_len = max(line_len, p);
            }

            for (vertical_pos, annotation) in &annotation_pos {
                // Draw lines of code
                for lineno in (annotation.position_rel.0).0..=(annotation.position_rel.1).0 {
                    writer_pos.0 = start_line + line_offset + 1;
                    writer_pos.1 = 0;

                    // draw line
                    if !(printed_lines.contains_key(annotation.filename)
                        && printed_lines
                            .get(annotation.filename)
                            .unwrap_or(&HashSet::new())
                            .contains(&lineno))
                    {
                        self.draw_line(
                            writer_pos,
                            annotation,
                            max_line_size,
                            span_thickness,
                            lineno,
                            &mut prev_line,
                            &mut printed_lines,
                            first,
                            &mut line_offset,
                            &vertical_annotations,
                        );
                    }

                    let contains_key = vertical_annotations.contains_key(&lineno);
                    if lineno == (annotation.position_rel.0).0 || contains_key {
                        if !self.is_multiline(&annotation) {
                            // Single line annotation
                            //
                            //   --> examples/tests.fluo:5:1
                            //    |
                            //  1 |   def entry() {
                            //  2 |       let x: int = 10+10*(1929+10);
                            //    |       ^^^ Error    ------------
                            //    |           that     |     | other error
                            //    |       overflows    | other error that goes over but its fine

                            *writer_pos = self.add_pipe(
                                writer_pos.0 - 1,
                                max_line_size,
                                &vertical_annotations,
                                lineno,
                                false,
                            );
                            *writer_pos = self.buffer.writel(
                                if prev_line_2 == lineno {
                                    writer_pos.0 - 1
                                } else {
                                    writer_pos.0
                                },
                                writer_pos.1 + span_thickness + (annotation.position_rel.0).1,
                                &annotation.mode.get_underline().repeat(
                                    (annotation.position_rel.1).1 - (annotation.position_rel.0).1,
                                ),
                                Style::new(
                                    Some(annotation.mode.get_color_class()),
                                    Some(Font::BOLD),
                                ),
                            );
                            if vertical_pos == &0 {
                                *writer_pos = self.buffer.writel(
                                    writer_pos.0 - 1,
                                    writer_pos.1,
                                    &annotation.message.as_ref().unwrap_or(&"".to_string()),
                                    Style::new(
                                        Some(annotation.mode.get_color_class()),
                                        Some(Font::BOLD),
                                    ),
                                );
                            } else {
                                for _ in 0..vertical_pos + 1 {
                                    writer_pos.1 = 0;
                                    *writer_pos = self.add_pipe(
                                        writer_pos.0,
                                        max_line_size,
                                        &vertical_annotations,
                                        lineno,
                                        false,
                                    );
                                    *writer_pos = self.buffer.writel(
                                        writer_pos.0 - 1,
                                        writer_pos.1
                                            + span_thickness
                                            + (annotation.position_rel.0).1,
                                        "|",
                                        Style::new(Some(annotation.mode.get_color_class()), None),
                                    );
                                }

                                *writer_pos = self.buffer.writel(
                                    writer_pos.0 - 1,
                                    writer_pos.1,
                                    &annotation.message.as_ref().unwrap_or(&"".to_string()),
                                    Style::new(
                                        Some(annotation.mode.get_color_class()),
                                        Some(Font::BOLD),
                                    ),
                                );
                            }

                            if Some(vertical_pos)
                                == match annotation_pos.len() {
                                    0 => None,
                                    n => Some(&annotation_pos[n - 1].0),
                                }
                            {
                                line_offset += if vertical_pos == &0 {
                                    self.add_pipe(
                                        start_line + line_offset,
                                        max_line_size,
                                        &vertical_annotations,
                                        lineno,
                                        false,
                                    );
                                    self.add_pipe(
                                        start_line + line_offset + 1,
                                        max_line_size,
                                        &vertical_annotations,
                                        lineno,
                                        false,
                                    );
                                    2usize
                                } else {
                                    self.add_pipe(
                                        start_line + line_offset + vertical_pos + 2,
                                        max_line_size,
                                        &vertical_annotations,
                                        lineno,
                                        false,
                                    );
                                    //self.add_pipe(start_line+line_offset+1, max_line_size);
                                    *vertical_pos + 3
                                };
                            }
                        }

                        // Multi-line annotation
                        if self.is_multiline(&annotation) || contains_key {
                            if contains_key {
                                // End of multiline
                                self.draw_last_multiline(
                                    writer_pos,
                                    vertical_annotations.get(&lineno).unwrap(),
                                    max_line_size,
                                    &mut line_offset,
                                    &vertical_annotations,
                                    lineno,
                                    span_thickness,
                                    span_no,
                                );
                                vertical_annotations.remove(&lineno);
                            } else if lineno == (annotation.position_rel.0).0 {
                                if vertical_pos == &0 {
                                    // Start of multiline
                                    if (annotation.position_rel.0).1 > 0 {
                                        writer_pos.0 = start_line + line_offset + 1;
                                        *writer_pos = self.add_pipe(
                                            writer_pos.0 - 1,
                                            max_line_size,
                                            &vertical_annotations,
                                            lineno,
                                            false,
                                        );
                                        *writer_pos = self.buffer.writel(
                                            writer_pos.0 - 1,
                                            self.indentation.len() * 2
                                                + 3
                                                + span_thickness
                                                + (annotation.position_rel.0).1,
                                            annotation.mode.get_underline(),
                                            Style::new(
                                                Some(annotation.mode.get_color_class()),
                                                Some(Font::BOLD),
                                            ),
                                        );
                                        writer_pos.0 += 1;
                                        line_offset += 1;
                                        self.buffer.writel(
                                            *vertical_pos + line_offset + start_line - 1,
                                            self.indentation.len() * 2
                                                + max_line_size
                                                + 3
                                                + span_no,
                                            &"_".repeat(
                                                span_thickness - span_no
                                                    + (annotation.position_rel.0).1
                                                    - 1,
                                            ),
                                            Style::new(
                                                Some(annotation.mode.get_color_class()),
                                                Some(Font::BOLD),
                                            ),
                                        );

                                        for _ in 0..span_thickness - vertical_pos {
                                            self.buffer.writech(
                                                writer_pos.0 - 1,
                                                self.indentation.len() * 2
                                                    + max_line_size
                                                    + 2
                                                    + span_no,
                                                '|',
                                                Style::new(
                                                    Some(annotation.mode.get_color_class()),
                                                    Some(Font::BOLD),
                                                ),
                                            );
                                        }

                                        vertical_annotations.insert(
                                            (annotation.position_rel.1).0,
                                            (*annotation).clone().clone(),
                                        );
                                        span_no += 1;
                                        if Some(vertical_pos)
                                            == match annotation_pos.len() {
                                                0 => None,
                                                n => Some(&annotation_pos[n - 1].0),
                                            }
                                        {
                                            self.add_pipe(
                                                start_line + line_offset + vertical_pos + 1,
                                                max_line_size,
                                                &vertical_annotations,
                                                lineno,
                                                false,
                                            );
                                            line_offset += *vertical_pos;
                                        }
                                    } else {
                                        *writer_pos = self.buffer.writech(
                                            writer_pos.0,
                                            writer_pos.1,
                                            '/',
                                            Style::new(
                                                Some(annotation.mode.get_color_class()),
                                                Some(Font::BOLD),
                                            ),
                                        );
                                    }
                                } else {
                                    // Start of multiline
                                    writer_pos.0 = start_line + line_offset + 1;
                                    for _ in 0..*vertical_pos {
                                        *writer_pos = self.add_pipe(
                                            writer_pos.0 - 1,
                                            max_line_size,
                                            &vertical_annotations,
                                            lineno,
                                            false,
                                        );

                                        *writer_pos = self.buffer.writel(
                                            writer_pos.0 - 1,
                                            self.indentation.len() * 2
                                                + 3
                                                + span_thickness
                                                + (annotation.position_rel.0).1,
                                            "|",
                                            Style::new(
                                                Some(annotation.mode.get_color_class()),
                                                Some(Font::BOLD),
                                            ),
                                        );
                                        writer_pos.0 += 1
                                    }

                                    self.buffer.writel(
                                        *vertical_pos + line_offset + start_line - 1,
                                        self.indentation.len() * 2 + max_line_size + 3 + span_no,
                                        &"_".repeat(
                                            span_thickness - span_no
                                                + (annotation.position_rel.0).1
                                                - 1,
                                        ),
                                        Style::new(
                                            Some(annotation.mode.get_color_class()),
                                            Some(Font::BOLD),
                                        ),
                                    );

                                    for i in 0..span_thickness - vertical_pos {
                                        self.buffer.writech(
                                            writer_pos.0 - 1 + i,
                                            self.indentation.len() * 2
                                                + max_line_size
                                                + 2
                                                + span_no,
                                            '|',
                                            Style::new(
                                                Some(annotation.mode.get_color_class()),
                                                Some(Font::BOLD),
                                            ),
                                        );
                                    }

                                    vertical_annotations.insert(
                                        (annotation.position_rel.1).0,
                                        (*annotation).clone().clone(),
                                    );
                                    span_no += 1;
                                    if Some(vertical_pos)
                                        == match annotation_pos.len() {
                                            0 => None,
                                            n => Some(&annotation_pos[n - 1].0),
                                        }
                                    {
                                        self.add_pipe(
                                            start_line + line_offset + vertical_pos + 1,
                                            max_line_size,
                                            &vertical_annotations,
                                            lineno,
                                            false,
                                        );
                                        line_offset += *vertical_pos;
                                    }
                                }

                                break;
                            } else {
                                // In between multiline annotation
                                *writer_pos = self.buffer.writech(
                                    writer_pos.0 - 1,
                                    max_line_size + self.indentation.len() + 4,
                                    '|',
                                    Style::new(
                                        Some(annotation.mode.get_color_class()),
                                        Some(Font::BOLD),
                                    ),
                                );
                                writer_pos.0 += 1;
                            };
                        }
                        prev_line_2 = lineno;
                    }

                    if first {
                        first = false;
                    }
                }
            }
        }
        if !vertical_annotations.is_empty() {
            // Fill in last multiline annotations

            let mut vertical_annotations_sorted: Vec<(usize, ErrorAnnotation)> =
                vertical_annotations.clone().into_iter().collect();
            vertical_annotations_sorted.sort_by_key(|a| a.0);
            span_no = 0;
            for (lineno, annotation) in vertical_annotations_sorted {
                writer_pos.0 = start_line + line_offset + 1;
                self.draw_line(
                    writer_pos,
                    &&annotation,
                    max_line_size,
                    span_thickness,
                    lineno,
                    &mut prev_line,
                    &mut printed_lines,
                    first,
                    &mut line_offset,
                    &vertical_annotations,
                );
                writer_pos.0 = start_line + line_offset + 1;
                self.add_pipe(
                    writer_pos.0 - 1,
                    max_line_size,
                    &vertical_annotations,
                    lineno,
                    false,
                );
                self.draw_last_multiline(
                    writer_pos,
                    &annotation,
                    max_line_size,
                    &mut line_offset,
                    &vertical_annotations,
                    lineno,
                    span_thickness,
                    span_no,
                );
                span_no += 1;
                line_offset += 1;
            }
        }
    }

    fn add_filename_pos(
        &mut self,
        filename: &'a str,
        position: &((usize, usize), (usize, usize)),
        max_line_size: usize,
        writer_pos: &mut (usize, usize),
    ) -> (usize, usize) {
        // Adds file annotation:
        //
        //     --> example/tests.fluo:5:1
        // ____ |
        //  |
        // 4 space without lineno
        *writer_pos = self.buffer.writel(
            writer_pos.0 - 1,
            writer_pos.1 + max_line_size + self.indentation.len(),
            "--> ",
            Style::new(Some(Color::BLUE), Some(Font::BOLD)),
        );
        self.buffer.writeln(
            writer_pos.0 - 1,
            writer_pos.1 - 1,
            &format!("{}:{}:{}", &filename, (position.0).0, (position.0).1 - 1),
            Style::new(None, None),
        )
    }

    fn is_multiline(&mut self, annotation: &ErrorAnnotation) -> bool {
        (annotation.position_rel.0).0 != (annotation.position_rel.1).0
    }

    fn get_code(&mut self, mut annotations: Vec<ErrorAnnotation<'a>>, err_pos: Pos) {
        let first = annotations.first().unwrap();
        let max_line_size: usize = self.get_max_line_size(&annotations);

        let mut writer_pos: (usize, usize) = (1, 1);
        let position;
        let mut span_thickness: usize = 0;

        position = (
            self.get_lineno(err_pos.s, first.filename),
            self.get_lineno(err_pos.e, first.filename),
        );

        // Add filename + position annotation
        writer_pos =
            self.add_filename_pos(first.filename, &position, max_line_size, &mut writer_pos);

        // Add pipe for padding
        writer_pos = self.add_pipe_pure(writer_pos.0 - 1, max_line_size);
        writer_pos.0 += 1;

        for mut annotation in &mut annotations {
            annotation.position_rel = (
                self.get_lineno(annotation.position.s, annotation.filename),
                self.get_lineno(annotation.position.e, annotation.filename),
            );
            if self.is_multiline(annotation) {
                span_thickness += 1;
            }
        }

        self.format_line(annotations, &mut writer_pos, span_thickness, max_line_size);
    }

    fn raise_type(&mut self, errors: Vec<Error<'a>>, message_type: ErrorDisplayType) {
        if !errors.is_empty() {
            eprintln!(
                "{}{}{}{} {} Found:{}\n",
                message_type.get_color(),
                color::UNDERLINE,
                color::BOLD,
                errors.len(),
                if errors.len() == 1 {
                    message_type.singular()
                } else {
                    message_type.plural()
                },
                color::RESET
            );
        } else {
            return;
        }
        for error in errors {
            eprintln!(
                "{}{}{}{}{}: {}{}{}\n",
                self.indentation,
                color::BOLD,
                message_type.get_color(),
                error.error.as_str(),
                color::RESET,
                color::BOLD,
                error.message,
                color::RESET
            );
            self.get_code(error.annotations, error.position);
            eprintln!("{}", self.buffer.render());
            self.buffer.reset();
        }
    }

    /// Raises all the errors on the error vector.
    /// Note: doesn't exit out of the program.
    pub fn raise(&mut self) {
        let warnings: Vec<Error> = self
            .errors
            .iter()
            .cloned()
            .filter(|x| {
                if let ErrorDisplayType::Warning = x.mode {
                    true
                } else {
                    false
                }
            })
            .collect();

        self.raise_type(warnings, ErrorDisplayType::Warning);

        let errors: Vec<Error> = self
            .errors
            .iter()
            .cloned()
            .filter(|x| {
                if let ErrorDisplayType::Error = x.mode {
                    true
                } else {
                    false
                }
            })
            .collect();

        self.raise_type(errors, ErrorDisplayType::Error);
    }

    /// Static method for error that parses the furthest.
    /// Useful when you have multiple errors and want to know which one is the most accurate.
    pub fn longest(errors: Vec<Error>) -> Error {
        let mut errors = errors;
        errors.sort_by_key(|x| {
            (
                // Urgents have a greater priority (i.e. a scope error)
                if x.urgent { 1 } else { 0 },
                // If its urgent, the one first wins
                Reverse(if x.urgent { x.position.s } else { 0 }),
                // Otherwise classify as the end position (the error that parses the furthest)
                x.position.e,
            )
        });
        errors.last().unwrap().clone()
    }
}
