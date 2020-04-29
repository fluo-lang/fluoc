use crate::helpers;
use std::fmt;
use crate::logger::logger::{ Error, ErrorDisplayType, ErrorType };

/// EOF Character
const EOF_CHAR: char = '\0';

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
/// Type of tokens
pub enum TokenType {
    STRING(String),
    DEF,
    IMPORT,
    RETURN,
    LET,
    IMPL,
    PATTERN,

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
    DOLLAR,
    
    SEMI,
    COMMA,
    EOF,
    UNKNOWN(String),
    LINECOMMENT(usize),
    BLOCKCOMMENT(usize),
    WHITESPACE(usize)
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let result = match self {
            TokenType::STRING(_) => String::from("string"),
            TokenType::IDENTIFIER(_) => String::from("identifier"),
            TokenType::NUMBER(_) => String::from("integer"),
            TokenType::UNKNOWN(_) => String::from("unknown token"),
            
            other => format!("{}", Token { token: other.clone(), pos: helpers::Pos { s: 0, e: 0 } })
        };
        write!(f, "{}", result)
    }
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
            TokenType::UNKNOWN(val) => String::from(format!("unknown token `{}`", val)),

            TokenType::LINECOMMENT(_) => String::from("line comment"),
            TokenType::BLOCKCOMMENT(_) => String::from("block comment"),
            TokenType::WHITESPACE(_) => String::from("whitespace"),
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

        TokenType::PATTERN
        => 7,

        | TokenType::IMPORT 
        | TokenType::RETURN 
        => 6,
        
        TokenType::IMPL
        => 4,

        | TokenType::LET
        | TokenType::DEF 
        => 3,

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
        | TokenType::DOLLAR
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
pub struct Lexer<'a> {
    pub filename: &'a str,
    pub file_contents: &'a str,
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

impl <'a> Lexer<'a> {
    /// Return new lexer object.
    /// 
    /// Arguments
    /// * `filename` - the filename of the file to read
    pub fn new(filename: &'a str, file_contents: &'a str) -> Lexer<'a> {
        Lexer { 
            filename: filename, 
            file_contents: file_contents, 
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
        }
    }

    /// generate a unit test for the lexer
    pub fn get_unit_test(&mut self) {
        loop {
            let token = self.advance();

            match token {
                Ok(tok) => match tok.token {
                    TokenType::EOF => {
                        break;
                    }
                    _ => {
                        println!("assert_eq!(*l.advance().unwrap(), {:?});", tok);
                    }
                },
                Err(_) => { }
            }
        }
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
    fn get_next_tok_type(&mut self) -> Result<TokenType, Error<'a>> {
        let first_char = self.bump();

        let mut token_kind = match first_char {
            '-' => match self.first() {
                '-' => self.line_comment(),
                '>' => {
                    self.bump();
                    Ok(TokenType::ARROW)
                },
                _ => Ok(TokenType::SUB)
            },
            c if is_whitespace(c) => self.whitespace(),

            c if self.is_id_start(c) => self.identifier(),

            '0'..='9' => self.number(),

            '*' => Ok(TokenType::MUL),
            '+' => Ok(TokenType::ADD),
            '/' => match self.first() {
                '*' => self.block_comment(),
                _ => Ok(TokenType::DIV)
            },

            '%' => match self.first() { 
                '%' => {
                    self.bump();
                    Ok(TokenType::DMOD)
                }
                _ => Ok(TokenType::MOD)
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
                _ => Ok(TokenType::COLON)
            },

            '"' => { self.string() },
            
            EOF_CHAR => Ok(TokenType::EOF),
            
            unknown => Err(Error::new( 
                format!("Unknown character `{}`", unknown.to_string()),
                ErrorType::UnknownCharacter,
                helpers::Pos {
                    s: self.temp_pos-1,
                    e: self.temp_pos
                },
                ErrorDisplayType::Error,
                self.filename,
                Vec::new(),
                true
            ))
        };

        if let Ok(TokenType::EOF) = token_kind {
            // Don't increment if EOF char, leads to weird out of bounds errors in error reporting
            self.temp_pos -= 1;
        }
        
        if let Ok(TokenType::WHITESPACE(_)) | Ok(TokenType::LINECOMMENT(_)) | Ok(TokenType::BLOCKCOMMENT(_)) = token_kind {
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
                '-' => { self.line_comment()?; self.eat(); return Ok(()); },
                _ => {  }
            },
            '/' => match self.first() {
                '*' => { self.block_comment()?; self.eat(); return Ok(()); },
                _ => {  }
            },
            c if is_whitespace(c) => { self.whitespace()?; self.eat(); return Ok(()); },
            _ => { }
        };
        
        self.set_pos(pos);
        Ok(())
    }

    /// Set position of lexer: for use by the parser
    pub fn set_pos(&mut self, pos: (usize, usize)) {
        self.position = pos.0;
        self.temp_pos = pos.1;
    }

    #[allow(unused_must_use)]
    /// Set get of lexer: for use by the parser
    pub fn get_pos(&mut self) -> Result<(usize, usize), Error<'a>> {
        self.eat_whitespace()?;
        Ok((self.position, self.temp_pos))
    }

    /// Get next token in input stream, and advance
    pub fn advance(&mut self) -> Result<&Token, Error<'a>> {
        let token_kind = self.get_next_tok_type()?;
        let position = self.position;
        self.eat();

        self.current_token = Token {
            pos: helpers::Pos::new(
                position,
                self.position
            ),
            token: token_kind
        };

        Ok(&self.current_token)
    }

    /// Get next token in input stream, but don't advance
    pub fn peek(&mut self) -> Result<&Token, Error<'a>> {
        let token_kind = self.get_next_tok_type()?;

        self.next_token = Token {
            pos: helpers::Pos::new(
                if get_tok_length(&token_kind)+self.temp_pos > self.position { 0 } else { self.position-get_tok_length(&token_kind)+self.temp_pos }, 
                self.position+self.temp_pos
            ),
            token: token_kind
        };

        self.temp_pos = 0;

        Ok(&self.next_token)
    }

    /// Tokenize integer
    fn number(&mut self) -> Result<TokenType, Error<'a>> {
        let num = self.eat_while(|c| '0' <= c && c <= '9');
        Ok(TokenType::NUMBER(num.1))
    }

    /// Get start of ID (excluding number)
    fn is_id_start(&mut self, c: char) -> bool {
        ('a' <= c && c <= 'z')
            || ('A' <= c && c <= 'Z')
            || c == '_'
    }
    
    /// Tokenize identifier and keywords
    fn identifier(&mut self) -> Result<TokenType, Error<'a>> {
        let id = self.eat_while(|c| is_id_continue(c));
        match id.1.as_str() {
            "def" => Ok(TokenType::DEF),
            "let" => Ok(TokenType::LET),
            "impl" => Ok(TokenType::IMPL),
            "pattern" => Ok(TokenType::PATTERN),
            "return" => Ok(TokenType::RETURN),
            _ => Ok(TokenType::IDENTIFIER(id.1))
        }
    }

    /// Validate string
    fn string(&mut self) -> Result<TokenType, Error<'a>> {
        let pos = self.position-1;
        let mut string = String::new();
        string.push('"');
        string.push(self.bump());
        
        let mut c = self.bump();

        while c != EOF_CHAR {
            match c {
                '"' => {
                    string.push(c);
                    return Ok(TokenType::STRING(string));
                }
                '\\' if self.first() == '\\' || self.first() == '"' => {
                    // Bump again to skip escaped character
                    self.bump();
                    string.push(c);
                }
                _ => {
                    string.push(c);
                },
            }
            c = self.bump();
        }

        Err(Error::new( 
            "Unterminated string".to_string(),
            ErrorType::UnterminatedString,
            helpers::Pos {
                s: pos,
                e: self.temp_pos
            },
            ErrorDisplayType::Error,
            self.filename,
            Vec::new(),
            true
        ))
    }

    /// Tokenize block comment
    fn block_comment(&mut self) -> Result<TokenType, Error<'a>> {
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
        if depth != 0 {
            
        }

        Ok(TokenType::BLOCKCOMMENT(self.position-position))
    }

    /// Tokenize line comment
    fn line_comment(&mut self) -> Result<TokenType, Error<'a>> {
        self.bump();
        Ok(TokenType::LINECOMMENT(self.eat_while(|c| c != '\n').0 + 1))
    }

    /// Tokenize whitespace
    fn whitespace(&mut self) -> Result<TokenType, Error<'a>> {
        Ok(TokenType::WHITESPACE(self.eat_while(is_whitespace).0 + 1))
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
    use TokenType::*;
    use crate::helpers::Pos;

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
    fn lex_test() -> Result<(), ()> {
        let mut l = Lexer::new(
            "./tests/lexer_test.fluo", 
r#"-- lexer test (code shouldn't work)

def entry(one, two) {
    let x: int = 10;

    -- hello work 12342824\n924910djwi2wkfjar2riar
    /* ajwd asjf*/
    return 1+192*20/(120 + "2319")%10/203911234567890;



    let _qwertyuiopasdfghjklzxcvbnm;
    _qwertyuiopasdfghjklzxcvbnm = "hi";

}

/*
hlleoa1234567890qwertyuiopasdfghjklzxcvbnm,./?><;'":[]\|}{=-0987654321`~!@#$%^&*()_+
one two
djawd
sfghsdjajdksajfiwjijfa
*/

def _123awfawjfaifjaiwjf(one, two) {


}
"#
        );
    
        assert_eq!(*l.advance().unwrap(), Token { token: DEF, pos: Pos { s: 37, e: 40 } });
        assert_eq!(*l.advance().unwrap(), Token { token: IDENTIFIER("entry".to_string()), pos: Pos { s: 41, e: 46 } });
        assert_eq!(*l.advance().unwrap(), Token { token: LP, pos: Pos { s: 46, e: 47 } });
        assert_eq!(*l.advance().unwrap(), Token { token: IDENTIFIER("one".to_string()), pos: Pos { s: 47, e: 50 } });
        assert_eq!(*l.advance().unwrap(), Token { token: COMMA, pos: Pos { s: 50, e: 51 } });
        assert_eq!(*l.advance().unwrap(), Token { token: IDENTIFIER("two".to_string()), pos: Pos { s: 52, e: 55 } });
        assert_eq!(*l.advance().unwrap(), Token { token: RP, pos: Pos { s: 55, e: 56 } });
        assert_eq!(*l.advance().unwrap(), Token { token: LCP, pos: Pos { s: 57, e: 58 } });
        assert_eq!(*l.advance().unwrap(), Token { token: LET, pos: Pos { s: 63, e: 66 } });
        assert_eq!(*l.advance().unwrap(), Token { token: IDENTIFIER("x".to_string()), pos: Pos { s: 67, e: 68 } });
        assert_eq!(*l.advance().unwrap(), Token { token: COLON, pos: Pos { s: 68, e: 69 } });
        assert_eq!(*l.advance().unwrap(), Token { token: IDENTIFIER("int".to_string()), pos: Pos { s: 70, e: 73 } });
        assert_eq!(*l.advance().unwrap(), Token { token: EQUALS, pos: Pos { s: 74, e: 75 } });
        assert_eq!(*l.advance().unwrap(), Token { token: NUMBER("10".to_string()), pos: Pos { s: 76, e: 78 } });
        assert_eq!(*l.advance().unwrap(), Token { token: SEMI, pos: Pos { s: 78, e: 79 } });
        assert_eq!(*l.advance().unwrap(), Token { token: RETURN, pos: Pos { s: 155, e: 161 } });
        assert_eq!(*l.advance().unwrap(), Token { token: NUMBER("1".to_string()), pos: Pos { s: 162, e: 163 } });
        assert_eq!(*l.advance().unwrap(), Token { token: ADD, pos: Pos { s: 163, e: 164 } });
        assert_eq!(*l.advance().unwrap(), Token { token: NUMBER("192".to_string()), pos: Pos { s: 164, e: 167 } });
        assert_eq!(*l.advance().unwrap(), Token { token: MUL, pos: Pos { s: 167, e: 168 } });
        assert_eq!(*l.advance().unwrap(), Token { token: NUMBER("20".to_string()), pos: Pos { s: 168, e: 170 } });
        assert_eq!(*l.advance().unwrap(), Token { token: DIV, pos: Pos { s: 170, e: 171 } });
        assert_eq!(*l.advance().unwrap(), Token { token: LP, pos: Pos { s: 171, e: 172 } });
        assert_eq!(*l.advance().unwrap(), Token { token: NUMBER("120".to_string()), pos: Pos { s: 172, e: 175 } });
        assert_eq!(*l.advance().unwrap(), Token { token: ADD, pos: Pos { s: 176, e: 177 } });
        assert_eq!(*l.advance().unwrap(), Token { token: STRING("\"2319\"".to_string()), pos: Pos { s: 178, e: 184 } });
        assert_eq!(*l.advance().unwrap(), Token { token: RP, pos: Pos { s: 184, e: 185 } });
        assert_eq!(*l.advance().unwrap(), Token { token: MOD, pos: Pos { s: 185, e: 186 } });
        assert_eq!(*l.advance().unwrap(), Token { token: NUMBER("10".to_string()), pos: Pos { s: 186, e: 188 } });
        assert_eq!(*l.advance().unwrap(), Token { token: DIV, pos: Pos { s: 188, e: 189 } });
        assert_eq!(*l.advance().unwrap(), Token { token: NUMBER("203911234567890".to_string()), pos: Pos { s: 189, e: 204 } });
        assert_eq!(*l.advance().unwrap(), Token { token: SEMI, pos: Pos { s: 204, e: 205 } });
        assert_eq!(*l.advance().unwrap(), Token { token: LET, pos: Pos { s: 213, e: 216 } });
        assert_eq!(*l.advance().unwrap(), Token { token: IDENTIFIER("_qwertyuiopasdfghjklzxcvbnm".to_string()), pos: Pos { s: 217, e: 244 } });
        assert_eq!(*l.advance().unwrap(), Token { token: SEMI, pos: Pos { s: 244, e: 245 } });
        assert_eq!(*l.advance().unwrap(), Token { token: IDENTIFIER("_qwertyuiopasdfghjklzxcvbnm".to_string()), pos: Pos { s: 250, e: 277 } });
        assert_eq!(*l.advance().unwrap(), Token { token: EQUALS, pos: Pos { s: 278, e: 279 } });
        assert_eq!(*l.advance().unwrap(), Token { token: STRING("\"hi\"".to_string()), pos: Pos { s: 280, e: 284 } });
        assert_eq!(*l.advance().unwrap(), Token { token: SEMI, pos: Pos { s: 284, e: 285 } });
        assert_eq!(*l.advance().unwrap(), Token { token: RCP, pos: Pos { s: 287, e: 288 } });
        assert_eq!(*l.advance().unwrap(), Token { token: DEF, pos: Pos { s: 419, e: 422 } });
        assert_eq!(*l.advance().unwrap(), Token { token: IDENTIFIER("_123awfawjfaifjaiwjf".to_string()), pos: Pos { s: 423, e: 443 } });
        assert_eq!(*l.advance().unwrap(), Token { token: LP, pos: Pos { s: 443, e: 444 } });
        assert_eq!(*l.advance().unwrap(), Token { token: IDENTIFIER("one".to_string()), pos: Pos { s: 444, e: 447 } });
        assert_eq!(*l.advance().unwrap(), Token { token: COMMA, pos: Pos { s: 447, e: 448 } });
        assert_eq!(*l.advance().unwrap(), Token { token: IDENTIFIER("two".to_string()), pos: Pos { s: 449, e: 452 } });
        assert_eq!(*l.advance().unwrap(), Token { token: RP, pos: Pos { s: 452, e: 453 } });
        assert_eq!(*l.advance().unwrap(), Token { token: LCP, pos: Pos { s: 454, e: 455 } });
        assert_eq!(*l.advance().unwrap(), Token { token: RCP, pos: Pos { s: 458, e: 459 } });
        Ok(())
    }
}
