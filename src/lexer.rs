use crate::helpers;
use std::io;
use std::str::Chars;

const EOF_CHAR: char = '\0';

#[derive(Copy, Clone, Debug)]
pub enum TokenType<'a> {
    STRING(&'a str),
    INLINE(&'a str),
    FUNC,
    IMPORT,
    RETURN,
    LET,
    IDENTIFIER(&'a str),
    NUMBER(&'a str),
    DIV,
    MOD,
    MUL,
    ADD,
    SUB,
    COMMA,
    LP,
    RP,
    LCP,
    RCP,
    QUESTION,
    DOT,
    EQUALS,
    SEMI,
    EOF,
    UNKNOWN,
    LINECOMMENT(i64),
    BLOCKCOMMENT(i64),
    WHITESPACE(i64)
}

#[derive(Debug)]
pub struct Token<'a> {
    token: TokenType<'a>,
    pos: helpers::Pos
}

fn get_tok_length(tok: TokenType) -> i64 {
    match tok {
        | TokenType::STRING(val)
        | TokenType::INLINE(val)
        | TokenType::IDENTIFIER(val)
        | TokenType::NUMBER(val)
        => val.chars().count() as i64,

        | TokenType::LINECOMMENT(val)
        | TokenType::BLOCKCOMMENT(val)
        | TokenType::WHITESPACE(val)
        => val,
        
        TokenType::LET  => 3,
        TokenType::FUNC => 4,

        | TokenType::IMPORT 
        | TokenType::RETURN 
        => 6,

        | TokenType::UNKNOWN 
        | TokenType::DIV
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
        => 1,
        
        TokenType::EOF => 0
    }
}

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

#[derive(Debug)]
pub struct Lexer<'a> {
    pub filename: &'a str,
    pub file_contents: &'a str,
    input_chars: Chars<'a>,
    previous: char,
    pub position: i64,
    pub current_token: Option<Token<'a>>
}

impl Lexer<'_> {
    pub fn new<'a>(filename: &'a str) -> io::Result<Lexer<'a>> {
        let file_contents = helpers::read_file(&filename)?;

        Ok(Lexer { filename, file_contents: file_contents, input_chars: file_contents.chars(), previous: EOF_CHAR, position: 0, current_token: None })
    }

    fn nth_char(&mut self, n: i64) -> char {
        self.input_chars.nth(n as usize).unwrap_or(EOF_CHAR)
    }

    fn first(&mut self) -> char {
        self.nth_char(0)
    }

    fn second(&mut self) -> char {
        self.nth_char(1)
    }

    fn prev(&mut self) -> char {
        self.previous
    }

    fn is_eof(&self) -> bool {
        self.input_chars.as_str().is_empty()
    }

    fn bump(&mut self) -> char {
        self.previous = self.first();
        let c = self.input_chars.next().unwrap_or(EOF_CHAR);
        self.position += 1;
        c
    }

    fn get_next_tok<'a>(&'a mut self) {
        
        let first_char = self.bump();

        let token_kind = match first_char {
            '/' => match self.first() {
                '/' => self.line_comment(),
                // TODO: Implement block comments
                // '*' => self.block_comment(),
                _ => TokenType::DIV
            },
            c if is_whitespace(c) => self.whitespace(),

            c if self.is_id_start(c) => self.identifier(),

            '0'..='9' => self.number(),

            '*' => TokenType::MUL,
            '-' => TokenType::SUB,
            '+' => TokenType::ADD,
            '%' => TokenType::MOD,
            '(' => TokenType::RP,
            ')' => TokenType::LP,
            '{' => TokenType::RCP,
            '}' => TokenType::LCP,
            '.' => TokenType::DOT,
            ';' => TokenType::SEMI,
            '=' => TokenType::EQUALS,
            '?' => TokenType::QUESTION,
            ',' => TokenType::COMMA,
            
            _ => TokenType::UNKNOWN
        };

        println!("{:?}", token_kind);
        //let pos = helpers::Pos {}

        /*self.current_token = Some(
            Token {
                token,
                pos
            }
        );*/
    }

    fn number(&mut self) -> TokenType {
        let num = self.eat_while(|c| '0' <= c && c <= '9');
        TokenType::NUMBER(&num.1[..])
    }

    fn is_id_start(&mut self, c: char) -> bool {
        ('a' <= c && c <= 'z')
            || ('A' <= c && c <= 'Z')
            || c == '_'
    }

    fn is_id_continue(&mut self, c: char) -> bool {
        ('a' <= c && c <= 'z')
            || ('A' <= c && c <= 'Z')
            || ('0' <= c && c <= '9')
            || c == '_'
    }
    
    fn identifier(&mut self) -> TokenType {
        let id = self.eat_while(|c| self.is_id_continue(c));
        TokenType::IDENTIFIER(&id.1[..])
    }

    fn line_comment(&mut self) -> TokenType {
        self.bump();
        TokenType::LINECOMMENT(self.eat_while(|c| c != '\n').0 + 1)
    }

    fn whitespace(&mut self) -> TokenType {
        TokenType::WHITESPACE(self.eat_while(is_whitespace).0 + 1)
    }

    fn eat_while<F>(&mut self, mut predicate: F) -> (i64, String)
    where
        F: FnMut(char) -> bool,
    {
        let mut eaten: i64 = 0;
        let mut content = self.previous.to_string();
        while predicate(self.first()) && !self.is_eof() {
            eaten += 1;
            content.push(self.bump());
        }

        (eaten, content)
    }

    pub fn peek<'a>(&'a mut self) -> Token<'a> {
        match self.current_token {
            Some(tok) => tok,
            None => {
                self.get_next_tok();
                self.current_token.unwrap()
            }
        }
    }

    pub fn advance<'a>(&'a mut self) {
        if let Some(tok) = self.current_token {
            self.position += get_tok_length(tok.token);
        }
    }
}

#[cfg(test)]
mod lexer_tests {
    use super::*;

    #[test]
    fn token_len_bin_op() {
        assert_eq!(get_tok_length(TokenType::ADD), 1);
        assert_eq!(get_tok_length(TokenType::DIV), 1);
        assert_eq!(get_tok_length(TokenType::SUB), 1);
        assert_eq!(get_tok_length(TokenType::MUL), 1);
        assert_eq!(get_tok_length(TokenType::DIV), 1);
    }

    #[test]
    fn token_len_keywords() {
        assert_eq!(get_tok_length(TokenType::FUNC), 4);
        assert_eq!(get_tok_length(TokenType::IMPORT), 6);
        assert_eq!(get_tok_length(TokenType::RETURN), 6);
        assert_eq!(get_tok_length(TokenType::LET), 3);
    }

    #[test]
    fn token_len_literal() {
        assert_eq!(get_tok_length(TokenType::QUESTION), 1);
        assert_eq!(get_tok_length(TokenType::SEMI), 1);
        assert_eq!(get_tok_length(TokenType::RP), 1);
        assert_eq!(get_tok_length(TokenType::LP), 1);
        assert_eq!(get_tok_length(TokenType::RCP), 1);
        assert_eq!(get_tok_length(TokenType::LCP), 1);
        assert_eq!(get_tok_length(TokenType::DOT), 1);
        assert_eq!(get_tok_length(TokenType::EQUALS), 1);
        assert_eq!(get_tok_length(TokenType::COMMA), 1);
        assert_eq!(get_tok_length(TokenType::UNKNOWN), 1);
    }

    #[test]
    fn token_len_eof() {
        assert_eq!(get_tok_length(TokenType::EOF), 0);
    }
}
