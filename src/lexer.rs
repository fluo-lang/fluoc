use crate::helpers;
use crate::logger::logger::{Error, ErrorAnnotation, ErrorDisplayType, ErrorType};

use std::fmt;
use std::path;

/// EOF Character
const EOF_CHAR: char = '\0';

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
/// Type of tokens
pub enum TokenType<'a> {
    STRING(&'a str),
    DEF,
    TYPE,
    OVERLOAD,
    IMPL,
    PATTERN,

    UNIT,

    LET,
    AS,
    RETURN,

    PUBLIC,

    EXTERN,

    AT,

    IF,
    ELSE,

    IDENTIFIER(&'a str),
    NUMBER(&'a str),

    DIV,
    MOD,
    MUL,
    ADD,
    SUB,
    DMOD,

    ARROW,

    LP,
    RP,
    LCP,
    RCP,
    LB,
    RB,

    QUESTION,
    DOT,
    EQUALS,
    COLON,
    DOUBLECOLON,
    DOLLAR,
    SEMI,
    COMMA,
    EOF,

    BOOL(&'a str),

    UNKNOWN(&'a str),
    LINECOMMENT(usize),
    BLOCKCOMMENT(usize),
    WHITESPACE(usize),
}

impl fmt::Display for TokenType<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let result = match &self {
            TokenType::STRING(_) => "string".to_string(),
            TokenType::IDENTIFIER(_) => "identifier".to_string(),
            TokenType::NUMBER(_) => "integer".to_string(),
            TokenType::UNKNOWN(_) => "unknown token".to_string(),

            other => format!(
                "{}",
                Token {
                    token: **other,
                    pos: helpers::Pos::new(0, 0, path::Path::new(""))
                }
            ),
        };
        write!(f, "{}", result)
    }
}

#[derive(Debug, Clone, Copy)]
/// Token object
pub struct Token<'a> {
    pub token: TokenType<'a>,
    pub pos: helpers::Pos<'a>,
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let result = match &self.token {
            TokenType::STRING(val) => format!("string `{}`", val),
            TokenType::IDENTIFIER(val) => format!("identifier `{}`", val),
            TokenType::NUMBER(val) => format!("number `{}`", val),

            TokenType::BOOL(val) => format!(
                "bool literal `{}`",
                match *val {
                    "0" => "false",
                    "1" => "true",
                    _ => panic!("Bool was not of 0 or 1"),
                }
            ),

            TokenType::DEF => "keyword `def`".to_string(),
            TokenType::RETURN => "keyword `return`".to_string(),
            TokenType::LET => "keyword `let`".to_string(),
            TokenType::IMPL => "keyword `impl`".to_string(),
            TokenType::PATTERN => "keyword `pattern`".to_string(),
            TokenType::TYPE => "keyword `type`".to_string(),
            TokenType::PUBLIC => "keyword `public`".to_string(),
            TokenType::UNIT => "keyword `unit`".to_string(),
            TokenType::EXTERN => "keyword `extern`".to_string(),
            TokenType::IF => "keyword `if`".to_string(),
            TokenType::ELSE => "keyword `else`".to_string(),
            TokenType::OVERLOAD => "keyword `overload`".to_string(),

            TokenType::AS => "operator `as`".to_string(),
            TokenType::DIV => "operator `/`".to_string(),
            TokenType::MOD => "operator `%`".to_string(),
            TokenType::MUL => "operator `*`".to_string(),
            TokenType::ADD => "operator `+`".to_string(),
            TokenType::SUB => "operator `-`".to_string(),
            TokenType::DMOD => "operator `%%`".to_string(),
            TokenType::DOLLAR => "token `$`".to_string(),
            TokenType::AT => "token `@`".to_string(),

            TokenType::DOUBLECOLON => "token `::`".to_string(),

            TokenType::ARROW => "token `=>`".to_string(),

            TokenType::LP => "token `(`".to_string(),
            TokenType::RP => "token `)`".to_string(),
            TokenType::LCP => "token `{`".to_string(),
            TokenType::RCP => "token `}`".to_string(),
            TokenType::LB => "token `[`".to_string(),
            TokenType::RB => "token `]`".to_string(),

            TokenType::QUESTION => "token `?`".to_string(),
            TokenType::DOT => "token `.`".to_string(),
            TokenType::EQUALS => "token `=`".to_string(),
            TokenType::COLON => "token `:`".to_string(),
            TokenType::SEMI => "terminator `;`".to_string(),
            TokenType::COMMA => "token `,`".to_string(),
            TokenType::EOF => "end of file".to_string(),
            TokenType::UNKNOWN(val) => format!("unknown token `{}`", val),

            TokenType::LINECOMMENT(_) => "line comment".to_string(),
            TokenType::BLOCKCOMMENT(_) => "block comment".to_string(),
            TokenType::WHITESPACE(_) => "whitespace".to_string(),
        };
        write!(f, "{}", result)
    }
}

impl<'a> PartialEq for Token<'a> {
    fn eq(&self, other: &Token<'_>) -> bool {
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

#[derive(Debug, Clone)]
/// Lexer object
pub struct Lexer<'a> {
    pub filename: &'a path::Path,
    pub file_contents: &'a str,
    file_iter: std::iter::Peekable<std::str::Chars<'a>>,
    pub position: usize,
    pub current_token: Token<'a>,
    change_peek: bool,
}

/// Check if ID is continue
fn is_id_continue(c: char) -> bool {
    ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || ('0' <= c && c <= '9') || c == '_'
}

impl<'a> Lexer<'a> {
    /// Return new lexer object.
    ///
    /// Arguments
    /// * `filename` - the filename of the file to read
    pub fn new(filename: &'a path::Path, file_contents: &'a str) -> Lexer<'a> {
        Lexer {
            filename,
            file_contents,
            file_iter: file_contents.chars().peekable(),
            position: 0,
            current_token: Token {
                token: TokenType::EOF,
                pos: helpers::Pos::new(0, 0, filename),
            },
            change_peek: true,
        }
    }

    /// generate a unit test for the lexer
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
    fn peek_char(&mut self) -> char {
        *self.file_iter.peek().unwrap_or(&EOF_CHAR)
    }
    /// Move position forward
    fn bump(&mut self) -> char {
        let c = self.file_iter.nth(0).unwrap_or(EOF_CHAR);
        self.position += 1;
        c
    }

    /// Get next token in input stream's type
    fn get_next_tok_type(&mut self) -> Result<(TokenType<'a>, usize), Error<'a>> {
        let first_char = self.bump();
        let pos = self.position;
        Ok((
            match first_char {
                '-' => match self.peek_char() {
                    '-' => {
                        self.line_comment()?;
                        self.bump(); // Eat the \n
                        return self.get_next_tok_type();
                    }
                    '>' => {
                        self.bump();
                        TokenType::ARROW
                    }
                    _ => TokenType::SUB,
                },

                c if is_whitespace(c) => {
                    self.whitespace()?;
                    return self.get_next_tok_type();
                }

                c if self.is_id_start(c) => self.identifier()?,

                '0'..='9' => self.number()?,

                '*' => TokenType::MUL,
                '+' => TokenType::ADD,
                '/' => match self.peek_char() {
                    '*' => {
                        self.bump();
                        self.block_comment()?;
                        return self.get_next_tok_type();
                    }
                    _ => TokenType::DIV,
                },

                '%' => match self.peek_char() {
                    '%' => TokenType::DMOD,
                    _ => TokenType::MOD,
                },
                '$' => TokenType::DOLLAR,

                '(' => TokenType::LP,
                ')' => TokenType::RP,
                '{' => TokenType::LCP,
                '}' => TokenType::RCP,
                '[' => TokenType::LB,
                ']' => TokenType::RB,

                '.' => TokenType::DOT,
                ';' => TokenType::SEMI,
                '=' => TokenType::EQUALS,
                '?' => TokenType::QUESTION,
                ',' => TokenType::COMMA,
                ':' => match self.peek_char() {
                    ':' => {
                        self.bump();
                        TokenType::DOUBLECOLON
                    }
                    _ => TokenType::COLON,
                },
                '@' => TokenType::AT,

                '"' => self.string()?,
                EOF_CHAR => {
                    self.position -= 1;
                    TokenType::EOF
                }

                unknown => {
                    let pos = helpers::Pos {
                        s: self.position - 1,
                        e: self.position,
                        filename: self.filename,
                    };
                    return Err(Error::new(
                        format!("Unknown character `{}`", unknown.to_string()),
                        ErrorType::UnknownCharacter,
                        pos,
                        ErrorDisplayType::Error,
                        vec![ErrorAnnotation::new(None, pos, ErrorDisplayType::Error)],
                        true,
                    ));
                }
            },
            pos,
        ))
    }

    pub fn advance(&mut self) -> Result<Token<'a>, Error<'a>> {
        self.peek()?;
        Ok(self.eat())
    }

    pub fn eat(&mut self) -> Token<'a> {
        self.change_peek = true;
        self.current_token
    }

    /// Get next token in input stream, but don't advance
    pub fn peek(&mut self) -> Result<Token<'a>, Error<'a>> {
        if self.change_peek {
            let (token_kind, start_pos) = self.get_next_tok_type()?;
            let end_pos = self.position;

            self.current_token = Token {
                pos: helpers::Pos::new(start_pos - 1, end_pos, self.filename),
                token: token_kind,
            };

            self.change_peek = false
        }
        Ok(self.current_token)
    }

    /// Tokenize integer
    fn number(&mut self) -> Result<TokenType<'a>, Error<'a>> {
        self.position -= 1;
        let num = self.eat_while(|c| '0' <= c && c <= '9');
        self.position += 1;
        Ok(TokenType::NUMBER(num.1))
    }

    /// Get start of ID (excluding number)
    fn is_id_start(&mut self, c: char) -> bool {
        ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c == '_'
    }

    /// Tokenize identifier and keywords
    fn identifier(&mut self) -> Result<TokenType<'a>, Error<'a>> {
        self.position -= 1;
        let id = self.eat_while(is_id_continue);
        self.position += 1;

        match id.1 {
            "def" => Ok(TokenType::DEF),
            "let" => Ok(TokenType::LET),
            "impl" => Ok(TokenType::IMPL),
            "pattern" => Ok(TokenType::PATTERN),
            "return" => Ok(TokenType::RETURN),
            "as" => Ok(TokenType::AS),
            "type" => Ok(TokenType::TYPE),
            "public" => Ok(TokenType::PUBLIC),
            "unit" => Ok(TokenType::UNIT),
            "true" => Ok(TokenType::BOOL("1")),
            "false" => Ok(TokenType::BOOL("0")),
            "extern" => Ok(TokenType::EXTERN),
            "if" => Ok(TokenType::IF),
            "else" => Ok(TokenType::ELSE),
            "overload" => Ok(TokenType::OVERLOAD),
            _ => Ok(TokenType::IDENTIFIER(id.1)),
        }
    }

    /// Validate string
    fn string(&mut self) -> Result<TokenType<'a>, Error<'a>> {
        let pos = self.position;
        let mut c = self.bump();

        while c != EOF_CHAR {
            match c {
                '"' => {
                    return Ok(TokenType::STRING(
                        &self.file_contents[pos - 1..self.position],
                    ));
                }
                val => {
                    let first = self.peek_char();
                    if (first == '\\' || first == '"') && val == '\\' {
                        // Bump again to skip escaped character
                        self.bump();
                        self.bump();
                    }
                }
            }
            c = self.bump();
        }
        Err(Error::new(
            "Unterminated string".to_string(),
            ErrorType::UnterminatedString,
            helpers::Pos {
                s: pos - 1,
                e: self.position - 1,
                filename: self.filename,
            },
            ErrorDisplayType::Error,
            vec![ErrorAnnotation::new(
                None,
                helpers::Pos {
                    s: pos - 1,
                    e: self.position - 1,
                    filename: self.filename,
                },
                ErrorDisplayType::Error,
            )],
            true,
        ))
    }

    /// Tokenize block comment
    fn block_comment(&mut self) -> Result<TokenType<'a>, Error<'a>> {
        let position = self.position;

        let mut depth = 1usize;
        while self.peek_char() != EOF_CHAR {
            let c = self.bump();
            let o = self.peek_char();
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

        Ok(TokenType::BLOCKCOMMENT(self.position - position))
    }

    /// Tokenize line comment
    fn line_comment(&mut self) -> Result<TokenType<'a>, Error<'a>> {
        self.bump();
        Ok(TokenType::LINECOMMENT(self.eat_while(|c| c != '\n').0 + 2))
    }

    /// Tokenize whitespace
    fn whitespace(&mut self) -> Result<TokenType<'a>, Error<'a>> {
        Ok(TokenType::WHITESPACE(self.eat_while(is_whitespace).0 + 1))
    }

    /// Eat while condition is true utility
    fn eat_while<F>(&mut self, mut predicate: F) -> (usize, &'a str)
    where
        F: FnMut(char) -> bool,
    {
        let mut eaten: usize = 0;
        let pos = self.position;
        let mut val = self.peek_char();
        while predicate(val) && val != EOF_CHAR {
            self.bump();
            eaten += 1;
            val = self.peek_char();
        }
        (
            eaten,
            if val != EOF_CHAR {
                &self.file_contents[pos..pos + eaten + 1]
            } else {
                &self.file_contents[pos..pos + eaten]
            },
        )
    }
}
