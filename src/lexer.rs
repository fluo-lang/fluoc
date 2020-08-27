use crate::helpers;
use crate::logger::{ErrorAnnotation, ErrorDisplayType, ErrorType, ErrorValue};
use crate::segmentation::{Grapheme, GraphemeIdxs};
use crate::sourcemap::SourceMap;

use std::borrow::Cow;
use std::rc::Rc;

const EOF: char = '\0';
const EOF_STR: &'static str = "\0";

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
/// Type of tokens
pub enum TokenType {
    String,
    CodeValue,

    Def,
    Type,
    Overload,
    Impl,
    Pattern,

    Unit,

    Let,
    As,

    Return,
    Yield,

    Public,

    Extern,

    At,

    If,
    Else,

    Identifier,
    Number,

    Div,
    Mod,
    Mul,
    Add,
    Sub,
    DMod,

    GT, // > Greater than
    LT, // < Less than
    GE, // >= Greater than or equal to
    LE, // <= Less than or equal to
    EQ, // == Equal to

    Arrow,

    LP,
    RP,
    LCP,
    RCP,
    LB,
    RB,

    Question,
    Dot,
    Equals,
    Colon,
    DoubleColon,
    Dollar,
    Semi,
    Comma,
    EOF,

    True,
    False,

    Unknown,
    LineComment(usize),
    BlockComment(usize),
    Whitespace(usize),
}

impl TokenType {
    /// Format with no sourcemap
    pub fn f(&self) -> &'static str {
        match self {
            TokenType::True => "bool literal",
            TokenType::False => "bool literal",

            TokenType::Def => "keyword `def`",
            TokenType::Return => "keyword `return`",
            TokenType::Yield => "keyword `yield`",
            TokenType::Let => "keyword `let`",
            TokenType::Impl => "keyword `impl`",
            TokenType::Pattern => "keyword `pattern`",
            TokenType::Type => "keyword `type`",
            TokenType::Public => "keyword `public`",
            TokenType::Unit => "keyword `unit`",
            TokenType::Extern => "keyword `extern`",
            TokenType::If => "keyword `if`",
            TokenType::Else => "keyword `else`",
            TokenType::Overload => "keyword `overload`",

            TokenType::As => "operator `as`",
            TokenType::Div => "operator `/`",
            TokenType::Mod => "operator `%`",
            TokenType::Mul => "operator `*`",
            TokenType::Add => "operator `+`",
            TokenType::Sub => "operator `-`",
            TokenType::DMod => "operator `%%`",
            TokenType::GT => "operator `>`",
            TokenType::LT => "operator `<`",
            TokenType::GE => "operator `>=`",
            TokenType::LE => "operator `<=`",
            TokenType::EQ => "operator `==`",

            TokenType::Dollar => "token `$`",
            TokenType::At => "token `@`",

            TokenType::DoubleColon => "token `::`",

            TokenType::Arrow => "token `=>`",

            TokenType::LP => "token `(`",
            TokenType::RP => "token `)`",
            TokenType::LCP => "token `{`",
            TokenType::RCP => "token `}`",
            TokenType::LB => "token `[`",
            TokenType::RB => "token `]`",

            TokenType::Question => "token `?`",
            TokenType::Dot => "token `.`",
            TokenType::Equals => "token `=`",
            TokenType::Colon => "token `:`",
            TokenType::Semi => "terminator `;`",
            TokenType::Comma => "token `,`",
            TokenType::EOF => "end of file",

            TokenType::LineComment(_) => "line comment",
            TokenType::BlockComment(_) => "block comment",
            TokenType::Whitespace(_) => "whitespace",

            TokenType::String => "string literal",
            TokenType::CodeValue => "code value",
            TokenType::Identifier => "identifier",
            TokenType::Number => "number",
            TokenType::Unknown => "unknown token",
        }
    }
}

#[derive(Debug, Clone, Copy)]
/// Token object
pub struct Token {
    pub token: TokenType,
    pub pos: helpers::Pos,
}

impl Token {
    pub fn f(&self, sourcemap: SourceMap) -> Cow<'static, str> {
        Cow::Owned(match self.token {
            TokenType::String => format!(
                "string literal `{}`",
                sourcemap.borrow().get_segment(self.pos)
            ),
            TokenType::CodeValue => {
                format!("code value `{}`", sourcemap.borrow().get_segment(self.pos))
            }
            TokenType::Identifier => {
                format!("identifier `{}`", sourcemap.borrow().get_segment(self.pos))
            }
            TokenType::Number => format!("number `{}`", sourcemap.borrow().get_segment(self.pos)),

            TokenType::Unknown => format!(
                "unknown token `{}`",
                sourcemap.borrow().get_segment(self.pos)
            ),
            val => return Cow::Borrowed(val.f()),
        })
    }
}

impl<'a> PartialEq for Token {
    fn eq(&self, other: &Token) -> bool {
        self.pos.e == other.pos.e && self.pos.s == other.pos.s && self.token == other.token
    }
}

/// Check if character is a whitespace character
pub fn is_whitespace(c: char) -> bool {
    match c {
        | '\u{0009}' // \t
            | '\u{000A}' // \n
            | '\u{000B}' // vertical tab
            | '\u{000C}' // form feed
            | '\u{000D}' // \r
            | '\u{0020}' // space

            // NEXT LINE from latin1
            | '\u{0085}'

            // Bidi markers
            | '\u{200E}' // LEFT-TO-RIGHT MARK
            | '\u{200F}' // RIGHT-TO-LEFT MARK

            // Dedicated whitespace characters from Unicode
            | '\u{2028}' // LINE SEPARATOR
            | '\u{2029}' // PARAGRAPH SEPARATOR
            => true,
        _ => false,
    }
}

#[derive(Clone)]
/// Lexer object
pub struct Lexer {
    pub filename: usize,
    pub position: usize,
    pub current_token: Token,

    unicode_iter: std::iter::Peekable<GraphemeIdxs>,

    change_peek: bool,
    sourcemap: SourceMap,
}

/// Check if ID is continue
fn is_id_continue(c: char) -> bool {
    ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || ('0' <= c && c <= '9') || c == '_'
}

impl Lexer {
    /// Return new lexer object.
    pub fn new(filename: usize, sourcemap: SourceMap) -> Lexer {
        Lexer {
            filename,
            position: 0,
            current_token: Token {
                token: TokenType::EOF,
                pos: helpers::Pos::new(0, 0, filename),
            },
            unicode_iter: GraphemeIdxs::new(Rc::clone(&sourcemap), filename).peekable(),
            change_peek: true,
            sourcemap,
        }
    }

    /// Generate a unit test for the lexer
    pub fn get_unit_test(&mut self) {
        loop {
            let token = self.peek();
            self.eat();

            if let Ok(tok) = token {
                match tok.token {
                    TokenType::EOF => {
                        break;
                    }
                    _ => {
                        println!("assert_eq!(*l.advance().unwrap(), {:?});", tok);
                    }
                }
            }
        }
    }

    /// Get next chat in input stream
    fn peek_char(&mut self) -> Grapheme {
        match self.unicode_iter.peek() {
            Some(grapheme) => grapheme.clone(),
            None => Grapheme::new(EOF_STR, 0), // EOF is 0 length
        }
    }

    /// Move position forward
    fn bump(&mut self) -> Grapheme {
        let c = match self.unicode_iter.next() {
            Some(grapheme) => grapheme.clone(),
            None => Grapheme::new(EOF_STR, 0), // EOF is 0 length
        };

        self.position += c.len();
        c
    }

    /// Get next token in input stream's type
    fn get_next_tok_type(&mut self) -> Result<(TokenType, usize), ErrorValue> {
        let first_char = self.bump();
        let pos = self.position;

        Ok((
            match first_char.front {
                '-' => match self.peek_char().front {
                    '-' => {
                        self.line_comment()?;
                        self.bump(); // Eat the \n
                        return self.get_next_tok_type();
                    }
                    '>' => {
                        self.bump();
                        TokenType::Arrow
                    }
                    _ => TokenType::Sub,
                },

                c if is_whitespace(c) => {
                    self.whitespace()?;
                    return self.get_next_tok_type();
                }

                c if self.is_id_start(c) => self.identifier()?,

                '0'..='9' => self.number()?,

                '*' => TokenType::Mul,
                '+' => TokenType::Add,
                '/' => match self.peek_char().front {
                    '*' => {
                        self.bump();
                        self.block_comment()?;
                        return self.get_next_tok_type();
                    }
                    _ => TokenType::Div,
                },

                '%' => match self.peek_char().front {
                    '%' => TokenType::DMod,
                    _ => TokenType::Mod,
                },
                '$' => TokenType::Dollar,
                '>' => match self.peek_char().front {
                    '=' => {
                        self.bump();
                        TokenType::GE
                    }
                    _ => TokenType::GT,
                },
                '<' => match self.peek_char().front {
                    '=' => {
                        self.bump();
                        TokenType::LE
                    }
                    _ => TokenType::LT,
                },

                '`' => self.string('`', "code value")?,

                '(' => TokenType::LP,
                ')' => TokenType::RP,
                '{' => TokenType::LCP,
                '}' => TokenType::RCP,
                '[' => TokenType::LB,
                ']' => TokenType::RB,

                '.' => TokenType::Dot,
                ';' => TokenType::Semi,
                '=' => match self.peek_char().front {
                    '=' => {
                        self.bump();
                        TokenType::EQ
                    }
                    '>' => {
                        self.bump();
                        TokenType::Arrow
                    }
                    _ => TokenType::Equals,
                },

                '?' => TokenType::Question,
                ',' => TokenType::Comma,
                ':' => match self.peek_char().front {
                    ':' => {
                        self.bump();
                        TokenType::DoubleColon
                    }
                    _ => TokenType::Colon,
                },
                '@' => TokenType::At,

                '"' => self.string('"', "string")?,
                EOF => TokenType::EOF,

                unknown => {
                    let pos = helpers::Pos {
                        s: self.position - 1,
                        e: self.position,
                        filename_id: self.filename,
                    };
                    return Err(ErrorValue::new(
                        format!("Unknown character `{}`", unknown.to_string()),
                        ErrorType::UnknownCharacter,
                        pos,
                        ErrorDisplayType::Error,
                        vec![ErrorAnnotation::new(None, pos, ErrorDisplayType::Error)],
                    ));
                }
            },
            pos,
        ))
    }

    pub fn advance(&mut self) -> Result<Token, ErrorValue> {
        self.peek()?;
        Ok(self.eat())
    }

    pub fn eat(&mut self) -> Token {
        self.change_peek = true;
        self.current_token
    }

    /// Get next token in input stream, but don't advance
    pub fn peek(&mut self) -> Result<Token, ErrorValue> {
        if self.change_peek {
            let (token_kind, mut start_pos) = self.get_next_tok_type()?;
            let end_pos = self.position;

            if start_pos == 0 { start_pos += 1; }

            self.current_token = Token {
                pos: helpers::Pos::new(start_pos - 1, end_pos, self.filename),
                token: token_kind,
            };

            self.change_peek = false
        }
        Ok(self.current_token)
    }

    /// Tokenize integer
    fn number(&mut self) -> Result<TokenType, ErrorValue> {
        let position = self.position;

        let num = self.eat_while(|c| '0' <= c && c <= '9');
        Ok(TokenType::Number)
    }

    /// Get start of ID (excluding number)
    fn is_id_start(&mut self, c: char) -> bool {
        ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c == '_'
    }

    /// Tokenize identifier and keywords
    fn identifier(&mut self) -> Result<TokenType, ErrorValue> {
        let position = self.position;
        let id = self.eat_while(is_id_continue);

        match self.sourcemap.borrow().get_segment(helpers::Pos::new(
            position - 1,
            position + id,
            self.filename,
        )) {
            "def" => Ok(TokenType::Def),
            "let" => Ok(TokenType::Let),
            "impl" => Ok(TokenType::Impl),
            "pattern" => Ok(TokenType::Pattern),
            "return" => Ok(TokenType::Return),
            "yield" => Ok(TokenType::Yield),
            "as" => Ok(TokenType::As),
            "type" => Ok(TokenType::Type),
            "public" => Ok(TokenType::Public),
            "unit" => Ok(TokenType::Unit),
            "true" => Ok(TokenType::True),
            "false" => Ok(TokenType::False),
            "extern" => Ok(TokenType::Extern),
            "if" => Ok(TokenType::If),
            "else" => Ok(TokenType::Else),
            "overload" => Ok(TokenType::Overload),
            _ => Ok(TokenType::Identifier),
        }
    }

    /// Validate string like
    fn string(&mut self, marker: char, name: &'static str) -> Result<TokenType, ErrorValue> {
        let pos = self.position;
        let mut c = self.bump().front;

        while c != EOF {
            match c {
                _ if c == marker => {
                    return Ok(TokenType::String);
                }
                val => {
                    let first = self.peek_char().front;
                    if (first == '\\' || first == marker) && val == '\\' {
                        // Bump again to skip escaped character
                        self.bump();
                        self.bump();
                    }
                }
            }
            c = self.bump().front;
        }

        let position_err = helpers::Pos {
            s: pos - 1,
            e: self.position - 1,
            filename_id: self.filename,
        };

        Err(ErrorValue::new(
            format!("Unterminated {}", name),
            ErrorType::UnterminatedString,
            position_err,
            ErrorDisplayType::Error,
            vec![ErrorAnnotation::new(
                None,
                position_err,
                ErrorDisplayType::Error,
            )],
        ))
    }

    /// Tokenize block comment
    fn block_comment(&mut self) -> Result<TokenType, ErrorValue> {
        let position = self.position;

        let mut depth = 1usize;
        while self.peek_char().front != EOF {
            let c = self.bump().front;
            let o = self.peek_char().front;
            match c {
                '/' if o == '*' => {
                    self.bump();
                    depth += 1;
                }
                '*' if o == '/' => {
                    self.bump();
                    depth -= 1;
                    if depth == 0 {
                        break;
                    }
                }
                _ => (),
            }
        }

        // We have eof
        // TODO: Error reporting: propagate back?
        if depth != 0 {}

        Ok(TokenType::BlockComment(self.position - position))
    }

    /// Tokenize line comment
    fn line_comment(&mut self) -> Result<TokenType, ErrorValue> {
        self.bump();
        Ok(TokenType::LineComment(self.eat_while(|c| c != '\n') + 2))
    }

    /// Tokenize whitespace
    fn whitespace(&mut self) -> Result<TokenType, ErrorValue> {
        Ok(TokenType::Whitespace(self.eat_while(is_whitespace) + 1))
    }

    /// Eat while condition is true utility
    fn eat_while<F>(&mut self, mut predicate: F) -> usize
    where
        F: FnMut(char) -> bool,
    {
        let mut eaten: usize = 0;
        let mut val = self.peek_char();
        while predicate(val.front) && val.front != EOF {
            self.bump();
            eaten += 1;
            val = self.peek_char();
        }
        eaten
    }
}

