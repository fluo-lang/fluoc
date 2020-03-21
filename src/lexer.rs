use crate::helpers;
use std::io;

const EOF_CHAR: char = '\0';

#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
    STRING(String),
    FUNC,
    IMPORT,
    RETURN,
    LET,
    IDENTIFIER(String),
    NUMBER(String),
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
    
    SEMI,
    COMMA,
    EOF,
    UNKNOWN,
    LINECOMMENT(i64),
    BLOCKCOMMENT(i64),
    WHITESPACE(i64)
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token: TokenType,
    pub pos: helpers::Pos
}

impl PartialEq<Token> for Token {
    fn eq(&self, other: &Token) -> bool {
        self.pos.e == other.pos.e 
        && self.pos.s == other.pos.s
        && self.token == other.token
    }
}

fn get_tok_length(tok: &TokenType) -> i64 {
    match tok {
        | TokenType::STRING(val)
        | TokenType::IDENTIFIER(val)
        | TokenType::NUMBER(val)
        => val.chars().count() as i64,

        | TokenType::LINECOMMENT(val)
        | TokenType::BLOCKCOMMENT(val)
        | TokenType::WHITESPACE(val)
        => *val,
        
        TokenType::LET  => 3,
        TokenType::FUNC => 4,

        | TokenType::IMPORT 
        | TokenType::RETURN 
        => 6,

        | TokenType::ARROW 
        | TokenType::DMOD => 2,

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
        | TokenType::COLON
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

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    pub filename: &'a str,
    pub file_contents: String,
    previous: char,
    pub position: i64,
    temp_pos: i64,
    next_token: Token,
    pub current_token: Token
}

fn is_id_continue(c: char) -> bool {
    ('a' <= c && c <= 'z')
        || ('A' <= c && c <= 'Z')
        || ('0' <= c && c <= '9')
        || c == '_'
}

impl Lexer<'_> {
    pub fn new(filename: &str) -> io::Result<Lexer> {
        let file_contents = helpers::read_file(&filename)?;
        Ok(Lexer { 
            filename, 
            file_contents, 
            previous: EOF_CHAR, 
            position: 0, 
            temp_pos: 0,
            current_token: Token { 
                token: TokenType::EOF, 
                pos: helpers::Pos {
                    s: 0, 
                    e: 0
                } 
            },
            next_token: Token { 
                token: TokenType::EOF, 
                pos: helpers::Pos {
                    s: 0, 
                    e: 0
                } 
            }
        })
    }

    fn nth_char(&mut self, n: i64) -> char {
        self.file_contents.chars().nth((n+self.position+self.temp_pos) as usize).unwrap_or(EOF_CHAR)
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
        if let None = self.file_contents.chars().nth(self.position as usize) {
            true
        } else {
            false
        }
    }

    fn bump(&mut self) -> char {
        self.previous = self.first();
        let c = self.file_contents.chars().nth((self.position+self.temp_pos) as usize).unwrap_or(EOF_CHAR);
        self.temp_pos += 1;
        c
    }

    fn eat(&mut self) {
        self.position += self.temp_pos;
        self.temp_pos = 0;
    }

    fn get_next_tok_type<'a>(&'a mut self) -> TokenType {
        let first_char = self.bump();

        let mut token_kind = match first_char {
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
            '%' => match self.first() { 
                '%' => {
                    self.bump();
                    TokenType::DMOD
                }
                _ => TokenType::MOD
            }
            '(' => TokenType::RP,
            ')' => TokenType::LP,
            '{' => TokenType::RCP,
            '}' => TokenType::LCP,
            '.' => TokenType::DOT,
            ';' => TokenType::SEMI,
            '=' => match self.first() {
                '>' => {
                    self.bump();
                    TokenType::ARROW
                }
                _ => TokenType::EQUALS,
            }
            '?' => TokenType::QUESTION,
            ',' => TokenType::COMMA,
            ':' => TokenType::COLON,

            EOF_CHAR => TokenType::EOF,
            
            _ => TokenType::UNKNOWN
        };
        
        if let TokenType::WHITESPACE(_) = token_kind {
            token_kind = self.get_next_tok_type()
        }
        token_kind
    }

    pub fn advance<'a>(&'a mut self) -> &Token {
        let token_kind = self.get_next_tok_type();
        self.eat();
        
        self.current_token = Token {
            pos: helpers::Pos {
                s: self.position-get_tok_length(&token_kind),
                e: self.position
            },
            token: token_kind
        };

        &self.current_token
    }

    pub fn peek<'a>(&'a mut self) -> &Token {
        if self.current_token != self.next_token {
            return &self.next_token;
        }

        let token_kind = self.get_next_tok_type();
        
        self.next_token = Token {
            pos: helpers::Pos {
                s: self.position-get_tok_length(&token_kind)+self.temp_pos,
                e: self.position+self.temp_pos
            },
            token: token_kind
        };

        self.temp_pos = 0;

        &self.next_token
    }

    fn number(&mut self) -> TokenType {
        let num = self.eat_while(|c| '0' <= c && c <= '9');
        TokenType::NUMBER(num.1)
    }

    fn is_id_start(&mut self, c: char) -> bool {
        ('a' <= c && c <= 'z')
            || ('A' <= c && c <= 'Z')
            || c == '_'
    }
    
    fn identifier(&mut self) -> TokenType {
        let id = self.eat_while(|c| is_id_continue(c));
        match id.1.as_str() {
            "func" => TokenType::FUNC,
            "let" => TokenType::LET,
            _ => TokenType::IDENTIFIER(id.1)
        }
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
        assert_eq!(get_tok_length(&TokenType::FUNC), 4);
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
        assert_eq!(get_tok_length(&TokenType::UNKNOWN), 1);
    }

    #[test]
    fn token_len_eof() {
        assert_eq!(get_tok_length(&TokenType::EOF), 0);
    }

    #[test]
    fn token_len_values() {
        assert_eq!(get_tok_length(&TokenType::NUMBER(String::from("10203"))), 5);
        assert_eq!(get_tok_length(&TokenType::NUMBER(String::from("1"))), 1);

        assert_eq!(get_tok_length(&TokenType::IDENTIFIER(String::from("hi_a2h8r"))), 8);
        assert_eq!(get_tok_length(&TokenType::IDENTIFIER(String::from("_"))), 1);

        assert_eq!(get_tok_length(&TokenType::STRING(String::from("\"10203\""))), 7);
        assert_eq!(get_tok_length(&TokenType::STRING(String::from("\"\""))), 2);
    }

    #[test]
    fn lex_test() -> io::Result<()> {
        let mut l = Lexer::new("./examples/simple_tests.fluo")?;
        assert_eq!(*l.advance(), Token { token: TokenType::FUNC, pos: helpers::Pos { s: 0, e: 4 } });
        assert_eq!(*l.advance(), Token { token: TokenType::IDENTIFIER(String::from("entry")), pos: helpers::Pos { s: 5, e: 10 } });
        assert_eq!(*l.advance(), Token { token: TokenType::RP, pos: helpers::Pos { s: 10, e: 11 } });
        assert_eq!(*l.advance(), Token { token: TokenType::LP, pos: helpers::Pos { s: 11, e: 12 } });
        assert_eq!(*l.advance(), Token { token: TokenType::RCP, pos: helpers::Pos { s: 13, e: 14 } });
        assert_eq!(*l.advance(), Token { token: TokenType::LET, pos: helpers::Pos { s: 19, e: 22 } });
        assert_eq!(*l.advance(), Token { token: TokenType::IDENTIFIER(String::from("x")), pos: helpers::Pos { s: 23, e: 24 } });
        assert_eq!(*l.advance(), Token { token: TokenType::COLON, pos: helpers::Pos { s: 24, e: 25 } });
        assert_eq!(*l.advance(), Token { token: TokenType::IDENTIFIER(String::from("int")), pos: helpers::Pos { s: 26, e: 29 } });
        assert_eq!(*l.advance(), Token { token: TokenType::SEMI, pos: helpers::Pos { s: 29, e: 30 } });
        assert_eq!(*l.advance(), Token { token: TokenType::LCP, pos: helpers::Pos { s: 31, e: 32 } });
        assert_eq!(*l.advance(), Token { token: TokenType::EOF, pos: helpers::Pos { s: 34, e: 34 } });
        Ok(())
    }
}
