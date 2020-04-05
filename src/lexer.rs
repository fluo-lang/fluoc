use crate::helpers;
use std::io;
use std::fmt;

/// EOF Character
const EOF_CHAR: char = '\0';

#[derive(Clone, Debug, PartialEq)]
/// Type of tokens
pub enum TokenType {
    STRING(String),
    DEF,
    IMPORT,
    RETURN,
    LET,
    IMPL,
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
    DOUBLECOLON,
    
    SEMI,
    COMMA,
    EOF,
    UNKNOWN(String),
    LINECOMMENT(usize),
    BLOCKCOMMENT(usize),
    WHITESPACE(usize)
}

#[derive(Debug, Clone)]
/// Token object
pub struct Token {
    pub token: TokenType,
    pub pos: helpers::Pos
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let result = match &self.token {
            TokenType::STRING(val) => format!("string `{}`", val.clone()),
            TokenType::IDENTIFIER(val) => format!("identifier `{}`", val.clone()),
            TokenType::NUMBER(val) => format!("number `{}`", val.clone()),

            TokenType::DEF => String::from("keyword `func`"),
            TokenType::IMPORT => String::from("keyword `import`"),
            TokenType::RETURN => String::from("keyword `return`"),
            TokenType::LET => String::from("keyword `let`"),
            TokenType::IMPL => String::from("keyword `impl`"),
            
            TokenType::DIV => String::from("operator `/`"),
            TokenType::MOD => String::from("operator `%`"),
            TokenType::MUL => String::from("operator `*`"),
            TokenType::ADD => String::from("operator `+`"),
            TokenType::SUB => String::from("operator `-`"),
            TokenType::DMOD => String::from("operator `%%`"),

            TokenType::DOUBLECOLON => String::from("double colon"),

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
            TokenType::UNKNOWN(val) => String::from(format!("unknown token `{}`", val)),

            TokenType::LINECOMMENT(_) => String::from("Line Comment"),
            TokenType::BLOCKCOMMENT(_) => String::from("Block Comment"),
            TokenType::WHITESPACE(_) => String::from("Whitespace"),
        };
        write!(f, "{}", result)
    }
}

impl PartialEq<Token> for Token {
    fn eq(&self, other: &Token) -> bool {
        self.pos.e == other.pos.e 
        && self.pos.s == other.pos.s
        && self.token == other.token
    }
}

/// Get length of token: useful for calculating positions of tokens
fn get_tok_length(tok: &TokenType) -> usize {
    match tok {
        | TokenType::STRING(val)
        | TokenType::IDENTIFIER(val)
        | TokenType::NUMBER(val)
        | TokenType::UNKNOWN(val)
        => val.chars().count(),

        | TokenType::LINECOMMENT(val)
        | TokenType::BLOCKCOMMENT(val)
        | TokenType::WHITESPACE(val)
        => *val,
        
        | TokenType::LET
        | TokenType::DEF 
        => 3,

        TokenType::IMPL
        => 4,

        | TokenType::IMPORT 
        | TokenType::RETURN 
        => 6,

        | TokenType::ARROW 
        | TokenType::DMOD
        | TokenType::DOUBLECOLON
        => 2,

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
pub struct Lexer {
    pub filename: String,
    pub file_contents: String,
    previous: char,
    pub position: usize,
    temp_pos: usize,
    next_token: Token,
    pub current_token: Token
}

/// Check if ID is continue
fn is_id_continue(c: char) -> bool {
    ('a' <= c && c <= 'z')
        || ('A' <= c && c <= 'Z')
        || ('0' <= c && c <= '9')
        || c == '_'
}

impl Lexer {
    /// Return new lexer object.
    /// 
    /// Arguments
    /// * `filename` - the filename of the file to read
    pub fn new(filename: String) -> io::Result<Lexer> {
        let file_contents = helpers::read_file(&filename)?;
        Ok(Lexer { 
            filename, 
            file_contents, 
            previous: EOF_CHAR, 
            position: 0, 
            temp_pos: 0,
            current_token: Token { 
                token: TokenType::EOF, 
                pos: helpers::Pos::new(0, 0)
            },
            next_token: Token { 
                token: TokenType::EOF, 
                pos: helpers::Pos::new(0, 0)
            }
        })
    }

    /// Get the nth char of input stream
    fn nth_char(&mut self, n: usize) -> char {
        self.file_contents.chars().nth(n+self.position+self.temp_pos).unwrap_or(EOF_CHAR)
    }

    /// Get next chat in input stream
    fn first(&mut self) -> char {
        self.nth_char(0)
    }

    /// Check if next value is EOF
    fn is_eof(&self) -> bool {
        if let None = self.file_contents.chars().nth(self.position as usize) {
            true
        } else {
            false
        }
    }

    /// Move position forward
    fn bump(&mut self) -> char {
        self.previous = self.first();
        let c = self.file_contents.chars().nth((self.position+self.temp_pos) as usize).unwrap_or(EOF_CHAR);
        self.temp_pos += 1;
        c
    }

    /// Eat up character from temp pos
    fn eat(&mut self) {
        self.position += self.temp_pos;
        self.temp_pos = 0;
    }

    /// Get next token in input stream's type
    fn get_next_tok_type<'a>(&'a mut self) -> TokenType {
        let first_char = self.bump();

        let mut token_kind = match first_char {
            '-' => match self.first() {
                '-' => self.line_comment(),
                '>' => {
                    self.bump();
                    TokenType::ARROW
                },
                _ => TokenType::SUB
            },
            c if is_whitespace(c) => self.whitespace(),

            c if self.is_id_start(c) => self.identifier(),

            '0'..='9' => self.number(),

            '*' => TokenType::MUL,
            '+' => TokenType::ADD,
            '/' => match self.first() {
                '*' => self.block_comment(),
                _ => TokenType::DIV
            },
            '%' => match self.first() { 
                '%' => {
                    self.bump();
                    TokenType::DMOD
                }
                _ => TokenType::MOD
            }
            '(' => TokenType::LP,
            ')' => TokenType::RP,
            '{' => TokenType::LCP,
            '}' => TokenType::RCP,
            '.' => TokenType::DOT,
            ';' => TokenType::SEMI,
            '=' => TokenType::EQUALS,
            '?' => TokenType::QUESTION,
            ',' => TokenType::COMMA,
            ':' => match self.first() {
                ':' => {
                    self.bump();
                    TokenType::DOUBLECOLON
                }
                _ => TokenType::COLON
            }

            EOF_CHAR => TokenType::EOF,
            
            unknown => TokenType::UNKNOWN(unknown.to_string())
        };
        
        if let TokenType::WHITESPACE(_) | TokenType::LINECOMMENT(_) | TokenType::BLOCKCOMMENT(_) = token_kind {
            token_kind = self.get_next_tok_type()
        }
        token_kind
    }

    /// Set position of lexer: for use by the parser
    pub fn set_pos(&mut self, pos: (usize, usize)) {
        self.position = pos.0;
        self.temp_pos = pos.1;
    }

    /// Set get of lexer: for use by the parser
    pub fn get_pos(&mut self) -> (usize, usize) {
        (self.position, self.temp_pos)
    }

    /// Get next token in input stream, and advance
    pub fn advance<'a>(&'a mut self) -> &Token {
        let token_kind = self.get_next_tok_type();
        self.eat();
        
        self.current_token = Token {
            pos: helpers::Pos::new(
                self.position-get_tok_length(&token_kind), 
                self.position
            ),
            token: token_kind
        };

        &self.current_token
    }

    /// Get next token in input stream, but don't advance
    pub fn peek<'a>(&'a mut self) -> &Token {
        let token_kind = self.get_next_tok_type();

        self.next_token = Token {
            pos: helpers::Pos::new(
                if get_tok_length(&token_kind)+self.temp_pos > self.position { 0 } else { self.position-get_tok_length(&token_kind)+self.temp_pos }, 
                self.position+self.temp_pos
            ),
            token: token_kind
        };

        self.temp_pos = 0;

        &self.next_token
    }

    /// Tokenize integer
    fn number(&mut self) -> TokenType {
        let num = self.eat_while(|c| '0' <= c && c <= '9');
        TokenType::NUMBER(num.1)
    }

    /// Get start of ID (excluding number)
    fn is_id_start(&mut self, c: char) -> bool {
        ('a' <= c && c <= 'z')
            || ('A' <= c && c <= 'Z')
            || c == '_'
    }
    
    /// Tokenize identifier and keywords
    fn identifier(&mut self) -> TokenType {
        let id = self.eat_while(|c| is_id_continue(c));
        match id.1.as_str() {
            "def" => TokenType::DEF,
            "let" => TokenType::LET,
            "impl" => TokenType::IMPL,
            _ => TokenType::IDENTIFIER(id.1)
        }
    }

    /// Tokenize block comment
    fn block_comment(&mut self) -> TokenType {
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
        if depth != 0 {
            
        }

        TokenType::BLOCKCOMMENT(self.position-position)
    }

    /// Tokenize line comment
    fn line_comment(&mut self) -> TokenType {
        self.bump();
        TokenType::LINECOMMENT(self.eat_while(|c| c != '\n').0 + 1)
    }

    /// Tokenize whitespace
    fn whitespace(&mut self) -> TokenType {
        TokenType::WHITESPACE(self.eat_while(is_whitespace).0 + 1)
    }

    /// Eat while condition is true utility
    fn eat_while<F>(&mut self, mut predicate: F) -> (usize, String)
    where
        F: FnMut(char) -> bool,
    {
        let mut eaten: usize = 0;
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
        assert_eq!(get_tok_length(&TokenType::NUMBER(String::from("10203"))), 5);
        assert_eq!(get_tok_length(&TokenType::NUMBER(String::from("1"))), 1);

        assert_eq!(get_tok_length(&TokenType::IDENTIFIER(String::from("hi_a2h8r"))), 8);
        assert_eq!(get_tok_length(&TokenType::IDENTIFIER(String::from("_"))), 1);

        assert_eq!(get_tok_length(&TokenType::STRING(String::from("\"10203\""))), 7);
        assert_eq!(get_tok_length(&TokenType::STRING(String::from("\"\""))), 2);

        assert_eq!(get_tok_length(&TokenType::UNKNOWN(String::from("!@#$%^"))), 6);
        assert_eq!(get_tok_length(&TokenType::UNKNOWN(String::from("`~"))), 2);
    }

    #[test]
    fn lex_test() -> io::Result<()> {
        let mut l = Lexer::new(String::from("./examples/simple_tests.fluo"))?;
        assert_eq!(*l.advance(), Token { token: TokenType::DEF, pos: helpers::Pos { s: 0, e: 3 } } ) ;
        assert_eq!(*l.advance(), Token { token: TokenType::IDENTIFIER(String::from("entry")), pos: helpers::Pos { s: 4, e: 9 } } ) ;
        assert_eq!(*l.advance(), Token { token: TokenType::LP, pos: helpers::Pos { s: 9, e: 10 } } ) ;
        assert_eq!(*l.advance(), Token { token: TokenType::RP, pos: helpers::Pos { s: 10, e: 11 } } ) ;
        assert_eq!(*l.advance(), Token { token: TokenType::LCP, pos: helpers::Pos { s: 12, e: 13 } } ) ;
        assert_eq!(*l.advance(), Token { token: TokenType::LET, pos: helpers::Pos { s: 18, e: 21 } } ) ;
        assert_eq!(*l.advance(), Token { token: TokenType::IDENTIFIER(String::from("x")), pos: helpers::Pos { s: 22, e: 23 } } ) ;
        assert_eq!(*l.advance(), Token { token: TokenType::COLON, pos: helpers::Pos { s: 23, e: 24 } } ) ;
        assert_eq!(*l.advance(), Token { token: TokenType::IDENTIFIER(String::from("int")), pos: helpers::Pos { s: 25, e: 28 } } ) ;
        assert_eq!(*l.advance(), Token { token: TokenType::EQUALS, pos: helpers::Pos { s: 29, e: 30 } } ) ;
        assert_eq!(*l.advance(), Token { token: TokenType::NUMBER(String::from("10")), pos: helpers::Pos { s: 31, e: 33 } } ) ;
        assert_eq!(*l.advance(), Token { token: TokenType::SEMI, pos: helpers::Pos { s: 33, e: 34 } } ) ;
        assert_eq!(*l.advance(), Token { token: TokenType::RCP, pos: helpers::Pos { s: 35, e: 36 } } ) ;
        assert_eq!(*l.advance(), Token { token: TokenType::EOF, pos: helpers::Pos { s: 38, e: 38 } } ) ;
        Ok(())
    }
}
