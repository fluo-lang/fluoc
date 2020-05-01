use crate::helpers;
use crate::logger::logger::{Error, ErrorDisplayType, ErrorType};
use std::fmt;

/// EOF Character
const EOF_CHAR: char = '\0';

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
/// Type of tokens
pub enum TokenType<'a> {
    STRING(&'a str),
    DEF,
    IMPORT,
    RETURN,
    LET,
    IMPL,
    PATTERN,

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

    QUESTION,
    DOT,
    EQUALS,
    COLON,
    DOUBLECOLON,
    DOLLAR,
    SEMI,
    COMMA,
    EOF,
    UNKNOWN(&'a str),
    LINECOMMENT(usize),
    BLOCKCOMMENT(usize),
    WHITESPACE(usize),
}

impl fmt::Display for TokenType<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let result = match &self {
            TokenType::STRING(_) => String::from("string"),
            TokenType::IDENTIFIER(_) => String::from("identifier"),
            TokenType::NUMBER(_) => String::from("integer"),
            TokenType::UNKNOWN(_) => String::from("unknown token"),

            other => format!(
                "{}",
                Token {
                    token: **other,
                    pos: helpers::Pos::new(0, 0)
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
    pub pos: helpers::Pos,
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let result = match &self.token {
            TokenType::STRING(val) => format!("string `{}`", val),
            TokenType::IDENTIFIER(val) => format!("identifier `{}`", val),
            TokenType::NUMBER(val) => format!("number `{}`", val),

            TokenType::DEF => String::from("keyword `func`"),
            TokenType::IMPORT => String::from("keyword `import`"),
            TokenType::RETURN => String::from("keyword `return`"),
            TokenType::LET => String::from("keyword `let`"),
            TokenType::IMPL => String::from("keyword `impl`"),
            TokenType::PATTERN => String::from("keyword `pattern`"),
            TokenType::DIV => String::from("operator `/`"),
            TokenType::MOD => String::from("operator `%`"),
            TokenType::MUL => String::from("operator `*`"),
            TokenType::ADD => String::from("operator `+`"),
            TokenType::SUB => String::from("operator `-`"),
            TokenType::DMOD => String::from("operator `%%`"),
            TokenType::DOLLAR => String::from("token `$`"),

            TokenType::DOUBLECOLON => String::from("token `::`"),

            TokenType::ARROW => String::from("token `=>`"),

            TokenType::LP => String::from("token `(`"),
            TokenType::RP => String::from("token `)`"),
            TokenType::LCP => String::from("token `{`"),
            TokenType::RCP => String::from("token `}`"),

            TokenType::QUESTION => String::from("token `?`"),
            TokenType::DOT => String::from("token `.`"),
            TokenType::EQUALS => String::from("token `=`"),
            TokenType::COLON => String::from("token `:`"),
            TokenType::SEMI => String::from("terminator `;`"),
            TokenType::COMMA => String::from("token `,`"),
            TokenType::EOF => String::from("end of file"),
            TokenType::UNKNOWN(val) => format!("unknown token `{}`", val),

            TokenType::LINECOMMENT(_) => String::from("line comment"),
            TokenType::BLOCKCOMMENT(_) => String::from("block comment"),
            TokenType::WHITESPACE(_) => String::from("whitespace"),
        };
        write!(f, "{}", result)
    }
}

impl<'a> PartialEq for Token<'a> {
    fn eq(&self, other: &Token) -> bool {
        self.pos.e == other.pos.e && self.pos.s == other.pos.s && self.token == other.token
    }
}

/// Get length of token: useful for calculating positions of tokens
fn get_tok_length(tok: &TokenType) -> usize {
    match tok {
        TokenType::STRING(val)
        | TokenType::IDENTIFIER(val)
        | TokenType::NUMBER(val)
        | TokenType::UNKNOWN(val) => val.chars().count(),

        TokenType::LINECOMMENT(val) | TokenType::BLOCKCOMMENT(val) | TokenType::WHITESPACE(val) => {
            *val
        }

        TokenType::PATTERN => 7,

        TokenType::IMPORT | TokenType::RETURN => 6,

        TokenType::IMPL => 4,

        TokenType::LET | TokenType::DEF => 3,

        TokenType::ARROW | TokenType::DMOD | TokenType::DOUBLECOLON => 2,

        TokenType::DIV
        | TokenType::MOD
        | TokenType::MUL
        | TokenType::ADD
        | TokenType::SUB
        | TokenType::COMMA
        | TokenType::LP
        | TokenType::RP
        | TokenType::LCP
        | TokenType::RCP
        | TokenType::QUESTION
        | TokenType::DOT
        | TokenType::EQUALS
        | TokenType::SEMI
        | TokenType::COLON
        | TokenType::DOLLAR => 1,

        TokenType::EOF => 0,
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

#[derive(Debug, Clone, Copy)]
/// Lexer object
pub struct Lexer<'a> {
    pub filename: &'a str,
    pub file_contents: &'a str,
    previous: char,
    pub position: usize,
    temp_pos: usize,
    next_token: Token<'a>,
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
    pub fn new(filename: &'a str, file_contents: &'a str) -> Lexer<'a> {
        Lexer {
            filename,
            file_contents,
            previous: EOF_CHAR,
            position: 0,
            temp_pos: 0,
            current_token: Token {
                token: TokenType::EOF,
                pos: helpers::Pos::new(0, 0),
            },
            next_token: Token {
                token: TokenType::EOF,
                pos: helpers::Pos::new(0, 0),
            },
            change_peek: true,
        }
    }

    /// generate a unit test for the lexer
    pub fn get_unit_test(&mut self) {
        loop {
            let token = self.advance();

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

    /// Get the nth char of input stream
    fn nth_char(&mut self, n: usize) -> char {
        self.file_contents
            .chars()
            .nth(n + self.position + self.temp_pos)
            .unwrap_or(EOF_CHAR)
    }

    /// Get next chat in input stream
    fn first(&mut self) -> char {
        self.nth_char(0)
    }

    /// Check if next value is EOF
    fn is_eof(&self) -> bool {
        self.file_contents
            .chars()
            .nth(self.position as usize)
            .is_none()
    }

    /// Move position forward
    fn bump(&mut self) -> char {
        self.previous = self.first();
        let c = self
            .file_contents
            .chars()
            .nth((self.position + self.temp_pos) as usize)
            .unwrap_or(EOF_CHAR);
        self.temp_pos += 1;
        c
    }

    /// Eat up character from temp pos
    fn eat(&mut self) {
        self.position += self.temp_pos;
        self.temp_pos = 0;
    }

    /// Get next token in input stream's type
    fn get_next_tok_type(&mut self) -> Result<TokenType<'a>, Error<'a>> {
        let first_char = self.bump();

        let mut token_kind = match first_char {
            '-' => match self.first() {
                '-' => self.line_comment(),
                '>' => {
                    self.bump();
                    Ok(TokenType::ARROW)
                }
                _ => Ok(TokenType::SUB),
            },
            c if is_whitespace(c) => self.whitespace(),

            c if self.is_id_start(c) => self.identifier(),

            '0'..='9' => self.number(),

            '*' => Ok(TokenType::MUL),
            '+' => Ok(TokenType::ADD),
            '/' => match self.first() {
                '*' => self.block_comment(),
                _ => Ok(TokenType::DIV),
            },

            '%' => match self.first() {
                '%' => {
                    self.bump();
                    Ok(TokenType::DMOD)
                }
                _ => Ok(TokenType::MOD),
            },
            '$' => Ok(TokenType::DOLLAR),

            '(' => Ok(TokenType::LP),
            ')' => Ok(TokenType::RP),
            '{' => Ok(TokenType::LCP),
            '}' => Ok(TokenType::RCP),
            '.' => Ok(TokenType::DOT),
            ';' => Ok(TokenType::SEMI),
            '=' => Ok(TokenType::EQUALS),
            '?' => Ok(TokenType::QUESTION),
            ',' => Ok(TokenType::COMMA),
            ':' => match self.first() {
                ':' => {
                    self.bump();
                    Ok(TokenType::DOUBLECOLON)
                }
                _ => Ok(TokenType::COLON),
            },

            '"' => self.string(),
            EOF_CHAR => Ok(TokenType::EOF),

            unknown => Err(Error::new(
                format!("Unknown character `{}`", unknown.to_string()),
                ErrorType::UnknownCharacter,
                helpers::Pos {
                    s: self.temp_pos - 1,
                    e: self.temp_pos,
                },
                ErrorDisplayType::Error,
                self.filename,
                Vec::new(),
                true,
            )),
        };

        if let Ok(TokenType::EOF) = token_kind {
            // Don't increment if EOF char, leads to weird out of bounds errors in error reporting
            self.temp_pos -= 1;
        }

        if let Ok(TokenType::WHITESPACE(_))
        | Ok(TokenType::LINECOMMENT(_))
        | Ok(TokenType::BLOCKCOMMENT(_)) = token_kind
        {
            self.eat();
            token_kind = self.get_next_tok_type();
        }

        token_kind
    }

    pub fn eat_whitespace(&mut self) -> Result<(), Error<'a>> {
        let pos = (self.position, self.temp_pos);
        let first_char = self.bump();

        match first_char {
            '-' => match self.first() {
                '-' => {
                    self.line_comment()?;
                    self.eat();
                    return Ok(());
                }
                _ => {}
            },
            '/' => match self.first() {
                '*' => {
                    self.block_comment()?;
                    self.eat();
                    return Ok(());
                }
                _ => {}
            },
            c if is_whitespace(c) => {
                self.whitespace()?;
                self.eat();
                return Ok(());
            }
            _ => {}
        };
        self.set_pos(pos);
        Ok(())
    }

    /// Set position of lexer: for use by the parser
    pub fn set_pos(&mut self, pos: (usize, usize)) {
        self.position = pos.0;
        self.temp_pos = pos.1;

        self.change_peek = true;
    }

    #[allow(unused_must_use)]
    /// Set get of lexer: for use by the parser
    pub fn get_pos(&mut self) -> Result<(usize, usize), Error<'a>> {
        self.eat_whitespace()?;
        Ok((self.position, self.temp_pos))
    }

    /// Get next token in input stream, and advance
    pub fn advance(&mut self) -> Result<Token<'a>, Error<'a>> {
        let token_kind = self.get_next_tok_type()?;
        let position = self.position;
        self.eat();

        self.current_token = Token {
            pos: helpers::Pos::new(position, self.position),
            token: token_kind,
        };

        self.change_peek = true;

        Ok(self.current_token)
    }

    /// Get next token in input stream, but don't advance
    pub fn peek(&mut self) -> Result<Token<'a>, Error<'a>> {
        if self.change_peek {
            let token_kind = self.get_next_tok_type()?;

            self.next_token = Token {
                pos: helpers::Pos::new(
                    if get_tok_length(&token_kind) + self.temp_pos > self.position {
                        0
                    } else {
                        self.position - get_tok_length(&token_kind) + self.temp_pos
                    },
                    self.position + self.temp_pos,
                ),
                token: token_kind,
            };

            self.temp_pos = 0;

            self.change_peek = false;

            Ok(self.next_token)
        } else {
            Ok(self.next_token)
        }
    }

    /// Tokenize integer
    fn number(&mut self) -> Result<TokenType<'a>, Error<'a>> {
        let num = self.eat_while(|c| '0' <= c && c <= '9');
        Ok(TokenType::NUMBER(num.1))
    }

    /// Get start of ID (excluding number)
    fn is_id_start(&mut self, c: char) -> bool {
        ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c == '_'
    }
    /// Tokenize identifier and keywords
    fn identifier(&mut self) -> Result<TokenType<'a>, Error<'a>> {
        let id = self.eat_while(is_id_continue);
        match id.1 {
            "def" => Ok(TokenType::DEF),
            "let" => Ok(TokenType::LET),
            "impl" => Ok(TokenType::IMPL),
            "pattern" => Ok(TokenType::PATTERN),
            "return" => Ok(TokenType::RETURN),
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
                        &self.file_contents[pos..self.temp_pos + pos],
                    ));
                }
                '\\' if self.first() == '\\' || self.first() == '"' => {
                    // Bump again to skip escaped character
                    self.bump();
                }
                _ => {}
            }
            c = self.bump();
        }

        Err(Error::new(
            "Unterminated string".to_string(),
            ErrorType::UnterminatedString,
            helpers::Pos {
                s: pos,
                e: self.temp_pos,
            },
            ErrorDisplayType::Error,
            self.filename,
            Vec::new(),
            true,
        ))
    }

    /// Tokenize block comment
    fn block_comment(&mut self) -> Result<TokenType<'a>, Error<'a>> {
        self.bump();
        let position = self.position;

        let mut depth = 1usize;
        while self.first() != EOF_CHAR {
            let c = self.first();
            self.bump();
            match c {
                '/' if self.first() == '*' => {
                    self.bump();
                    depth += 1;
                }
                '*' if self.first() == '/' => {
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
        Ok(TokenType::LINECOMMENT(self.eat_while(|c| c != '\n').0 + 1))
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
        while predicate(self.first()) && !self.is_eof() {
            eaten += 1;
            self.bump();
        }

        (eaten, &self.file_contents[pos..pos + eaten + 1])
    }
}

#[cfg(test)]
mod lexer_tests {
    use super::*;

    #[test]
    fn token_len_bin_op() {
        assert_eq!(get_tok_length(&TokenType::ADD), 1);
        assert_eq!(get_tok_length(&TokenType::DIV), 1);
        assert_eq!(get_tok_length(&TokenType::SUB), 1);
        assert_eq!(get_tok_length(&TokenType::MUL), 1);
        assert_eq!(get_tok_length(&TokenType::DIV), 1);
    }

    #[test]
    fn token_len_keywords() {
        assert_eq!(get_tok_length(&TokenType::DEF), 3);
        assert_eq!(get_tok_length(&TokenType::IMPORT), 6);
        assert_eq!(get_tok_length(&TokenType::RETURN), 6);
        assert_eq!(get_tok_length(&TokenType::LET), 3);
    }

    #[test]
    fn token_len_literal() {
        assert_eq!(get_tok_length(&TokenType::QUESTION), 1);
        assert_eq!(get_tok_length(&TokenType::SEMI), 1);
        assert_eq!(get_tok_length(&TokenType::RP), 1);
        assert_eq!(get_tok_length(&TokenType::LP), 1);
        assert_eq!(get_tok_length(&TokenType::RCP), 1);
        assert_eq!(get_tok_length(&TokenType::LCP), 1);
        assert_eq!(get_tok_length(&TokenType::DOT), 1);
        assert_eq!(get_tok_length(&TokenType::EQUALS), 1);
        assert_eq!(get_tok_length(&TokenType::COMMA), 1);
    }

    #[test]
    fn token_len_eof() {
        assert_eq!(get_tok_length(&TokenType::EOF), 0);
    }

    #[test]
    fn token_len_values() {
        assert_eq!(get_tok_length(&TokenType::NUMBER("10203")), 5);
        assert_eq!(get_tok_length(&TokenType::NUMBER("1")), 1);

        assert_eq!(get_tok_length(&TokenType::IDENTIFIER("hi_a2h8r")), 8);
        assert_eq!(get_tok_length(&TokenType::IDENTIFIER("_")), 1);

        assert_eq!(get_tok_length(&TokenType::STRING("\"10203\"")), 7);
        assert_eq!(get_tok_length(&TokenType::STRING("\"\"")), 2);

        assert_eq!(get_tok_length(&TokenType::UNKNOWN("!@#$%^")), 6);
        assert_eq!(get_tok_length(&TokenType::UNKNOWN("`~")), 2);
    }
}
