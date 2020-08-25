use crate::helpers;
use crate::logger::{ErrorValue, ErrorAnnotation, ErrorDisplayType, ErrorType};
use crate::segmentation::{Grapheme, GraphemeIdxs};
use crate::sourcemap::SourceMap;

use std::rc::Rc;

const EOF: char = '\0';
const EOF_STR: &'static str = "\0";

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
/// Type of tokens
pub enum TokenType {
    STRING,
    CODEVALUE,

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

    IDENTIFIER,
    NUMBER,

    DIV,
    MOD,
    MUL,
    ADD,
    SUB,
    DMOD,

    GT, // > Greater than
    LT, // < Less than
    GE, // >= Greater than or equal to
    LE, // <= Less than or equal to
    EQ, // == Equal to

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

    TRUE,
    FALSE,

    UNKNOWN,
    LINECOMMENT(usize),
    BLOCKCOMMENT(usize),
    WHITESPACE(usize),
}

impl TokenType {
    pub fn f(&self, sourcemap: SourceMap) -> String {
        match &self {
            TokenType::STRING => "string".to_string(),
            TokenType::IDENTIFIER => "identifier".to_string(),
            TokenType::NUMBER => "integer".to_string(),
            TokenType::UNKNOWN => "unknown token".to_string(),

            other => format!(
                "{}",
                Token {
                    token: **other,
                    pos: helpers::Pos::new(0, 0, 0)
                }
                .f(sourcemap)
            ),
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
    pub fn f(&self, sourcemap: SourceMap) -> String {
        match self.token {
            TokenType::STRING => format!("string `{}`", sourcemap.borrow().get_segment(self.pos)),
            TokenType::CODEVALUE => {
                format!("code value `{}`", sourcemap.borrow().get_segment(self.pos))
            }
            TokenType::IDENTIFIER => {
                format!("identifier `{}`", sourcemap.borrow().get_segment(self.pos))
            }
            TokenType::NUMBER => format!("number `{}`", sourcemap.borrow().get_segment(self.pos)),

            TokenType::TRUE => "bool literal `true`".to_string(),
            TokenType::FALSE => "bool literal `false`".to_string(),

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
            TokenType::GT => "operator `>`".to_string(),
            TokenType::LT => "operator `<`".to_string(),
            TokenType::GE => "operator `>=`".to_string(),
            TokenType::LE => "operator `<=`".to_string(),
            TokenType::EQ => "operator `==`".to_string(),

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
            TokenType::UNKNOWN => format!(
                "unknown token `{}`",
                sourcemap.borrow().get_segment(self.pos)
            ),

            TokenType::LINECOMMENT(_) => "line comment".to_string(),
            TokenType::BLOCKCOMMENT(_) => "block comment".to_string(),
            TokenType::WHITESPACE(_) => "whitespace".to_string(),
        }
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
    ///
    /// Arguments
    /// * `filename` - the filename of the file to read
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
                '/' => match self.peek_char().front {
                    '*' => {
                        self.bump();
                        self.block_comment()?;
                        return self.get_next_tok_type();
                    }
                    _ => TokenType::DIV,
                },

                '%' => match self.peek_char().front {
                    '%' => TokenType::DMOD,
                    _ => TokenType::MOD,
                },
                '$' => TokenType::DOLLAR,
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

                '.' => TokenType::DOT,
                ';' => TokenType::SEMI,
                '=' => match self.peek_char().front {
                    '=' => {
                        self.bump();
                        TokenType::EQ
                    }
                    _ => TokenType::EQUALS,
                },

                '?' => TokenType::QUESTION,
                ',' => TokenType::COMMA,
                ':' => match self.peek_char().front {
                    ':' => {
                        self.bump();
                        TokenType::DOUBLECOLON
                    }
                    _ => TokenType::COLON,
                },
                '@' => TokenType::AT,

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
                        true,
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
    fn number(&mut self) -> Result<TokenType, ErrorValue> {
        let position = self.position;

        let num = self.eat_while(|c| '0' <= c && c <= '9');
        Ok(TokenType::NUMBER)
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
            "def" => Ok(TokenType::DEF),
            "let" => Ok(TokenType::LET),
            "impl" => Ok(TokenType::IMPL),
            "pattern" => Ok(TokenType::PATTERN),
            "return" => Ok(TokenType::RETURN),
            "as" => Ok(TokenType::AS),
            "type" => Ok(TokenType::TYPE),
            "public" => Ok(TokenType::PUBLIC),
            "unit" => Ok(TokenType::UNIT),
            "true" => Ok(TokenType::TRUE),
            "false" => Ok(TokenType::FALSE),
            "extern" => Ok(TokenType::EXTERN),
            "if" => Ok(TokenType::IF),
            "else" => Ok(TokenType::ELSE),
            "overload" => Ok(TokenType::OVERLOAD),
            _ => Ok(TokenType::IDENTIFIER),
        }
    }

    /// Validate string like
    fn string(&mut self, marker: char, name: &'static str) -> Result<TokenType, ErrorValue> {
        let pos = self.position;
        let mut c = self.bump().front;

        while c != EOF {
            match c {
                _ if c == marker => {
                    return Ok(TokenType::STRING);
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
            true,
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

        Ok(TokenType::BLOCKCOMMENT(self.position - position))
    }

    /// Tokenize line comment
    fn line_comment(&mut self) -> Result<TokenType, ErrorValue> {
        self.bump();
        Ok(TokenType::LINECOMMENT(self.eat_while(|c| c != '\n') + 2))
    }

    /// Tokenize whitespace
    fn whitespace(&mut self) -> Result<TokenType, ErrorValue> {
        Ok(TokenType::WHITESPACE(self.eat_while(is_whitespace) + 1))
    }

    /// Eat while condition is true utility
    fn eat_while<F>(&mut self, mut predicate: F) -> usize
    where
        F: FnMut(char) -> bool,
    {
        let mut eaten: usize = 0;
        let pos = self.position;
        let mut val = self.peek_char();
        while predicate(val.front) && val.front != EOF {
            self.bump();
            eaten += 1;
            val = self.peek_char();
        }
        eaten
    }
}
