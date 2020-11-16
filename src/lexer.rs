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

    Def,
    Type,
    Impl,
    Pattern,

    Unit,

    Let,

    As,
    Is,

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
    FatArrow,

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
    Comma,
    EOF,

    /// Line terminator (not every newline)
    LineTerm,

    True,
    False,

    Underscore,

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
            TokenType::Public => "keyword `pub`",
            TokenType::Unit => "keyword `unit`",
            TokenType::Extern => "keyword `extern`",
            TokenType::If => "keyword `if`",
            TokenType::Else => "keyword `else`",

            TokenType::As => "operator `as`",
            TokenType::Is => "operator`is`",

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

            TokenType::At => "token `@`",

            TokenType::DoubleColon => "token `::`",

            TokenType::Arrow => "token `->`",
            TokenType::FatArrow => "token `=>`",

            TokenType::LP => "token `(`",
            TokenType::RP => "token `)`",
            TokenType::LCP => "token `{`",
            TokenType::RCP => "token `}`",
            TokenType::LB => "token `[`",
            TokenType::RB => "token `]`",

            TokenType::LineTerm => "line terminator",

            TokenType::Question => "token `?`",
            TokenType::Dot => "token `.`",
            TokenType::Equals => "token `=`",
            TokenType::Colon => "token `:`",
            TokenType::Comma => "token `,`",
            TokenType::EOF => "end of file",
            TokenType::Underscore => "underscore",

            TokenType::LineComment(_) => "line comment",
            TokenType::BlockComment(_) => "block comment",
            TokenType::Whitespace(_) => "whitespace",

            TokenType::String => "string literal",
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
    pub pos: helpers::Span,
}

impl Token {
    pub fn f(&self, sourcemap: SourceMap) -> Cow<'static, str> {
        Cow::Owned(match self.token {
            TokenType::String => format!(
                "string literal `{}`",
                sourcemap.borrow().get_segment(self.pos)
            ),
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
        '\u{0009}' // \t
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

    unicode_iter: std::iter::Peekable<GraphemeIdxs>,

    change_peek: bool,
    sourcemap: SourceMap,
    previous_token: Token,
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
            previous_token: Token {
                token: TokenType::EOF,
                pos: helpers::Span::new(0, 0, filename),
            },
            unicode_iter: GraphemeIdxs::new(Rc::clone(&sourcemap), filename).peekable(),
            change_peek: true,
            sourcemap,
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

                '\n' => {
                    if self.emit_line_break() {
                        TokenType::LineTerm
                    } else {
                        return self.get_next_tok_type();
                    }
                }

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
                    '%' => {
                        self.bump();
                        TokenType::DMod
                    }
                    _ => TokenType::Mod,
                },
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
                '=' => match self.peek_char().front {
                    '=' => {
                        self.bump();
                        TokenType::EQ
                    }
                    '>' => {
                        self.bump();
                        TokenType::FatArrow
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
                    let pos = helpers::Span {
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

    fn emit_line_break(&mut self) -> bool {
        // We break if the previous character was a valid ending:
        // RP, RCP, RB, Identifier, True, False, Number, String
        // Anything else is invalid, so don't insert an ending
        match self.previous_token.token {
            TokenType::RP
            | TokenType::RCP
            | TokenType::RB
            | TokenType::Identifier
            | TokenType::True
            | TokenType::False
            | TokenType::Number
            | TokenType::String => true,
            _ => false,
        }
    }

    pub fn get_tokens(&mut self) -> Result<Vec<Token>, ErrorValue> {
        let mut tokens = Vec::new();
        loop {
            let (token_kind, mut start_pos) = self.get_next_tok_type()?;
            let end_pos = self.position;

            if start_pos == 0 {
                start_pos += 1;
            }

            let token = Token {
                pos: helpers::Span::new(start_pos - 1, end_pos, self.filename),
                token: token_kind,
            };

            match token_kind {
                TokenType::EOF if self.previous_token.token != TokenType::LineTerm => {
                    let mut new_token = token;
                    new_token.token = TokenType::LineTerm;
                    tokens.push(new_token);
                }
                TokenType::RCP if self.emit_line_break() => {
                    let mut new_token = token;
                    new_token.token = TokenType::LineTerm;
                    tokens.push(new_token);
                }
                _ => {}
            }

            tokens.push(token);

            if token_kind == TokenType::EOF {
                break;
            }

            self.previous_token = token;
        }
        Ok(tokens)
    }

    /// Tokenize integer
    fn number(&mut self) -> Result<TokenType, ErrorValue> {
        self.eat_while(|c| '0' <= c && c <= '9');
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

        match self.sourcemap.borrow().get_segment(helpers::Span::new(
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
            "pub" => Ok(TokenType::Public),
            "unit" => Ok(TokenType::Unit),
            "true" => Ok(TokenType::True),
            "false" => Ok(TokenType::False),
            "extern" => Ok(TokenType::Extern),
            "if" => Ok(TokenType::If),
            "else" => Ok(TokenType::Else),
            "_" => Ok(TokenType::Underscore),
            "is" => Ok(TokenType::Is),
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

        let position_err = helpers::Span {
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

#[cfg(test)]
mod lexer_tests {
    use super::*;

    use crate::sourcemap::SourceMapInner;
    use std::path;

    macro_rules! lex_assert {
        ($source: expr, $token: expr, $name: ident) => {
            #[test]
            fn $name() {
                let sourcemap = SourceMapInner::new();
                let filename_id = sourcemap
                    .borrow_mut()
                    .insert_file(path::PathBuf::from("test_fl.fl"), $source.to_string());

                let mut l = Lexer::new(filename_id, sourcemap);
                let toks = l.get_tokens().unwrap();
                assert_eq!(toks[toks.len() - 3].token, $token)
            }
        };
    }

    lex_assert!(" true", TokenType::True, true_test);
    lex_assert!(" false", TokenType::False, false_test);

    lex_assert!(" \"hello \"", TokenType::String, string_test);
    lex_assert!(
        " \"12d8a9fh3nffandjs \\\" \"",
        TokenType::String,
        string_escape_test
    );

    lex_assert!(" def", TokenType::Def, def_test);
    lex_assert!(" type", TokenType::Type, type_test);
    lex_assert!(" impl", TokenType::Impl, impl_test);
    lex_assert!(" pattern", TokenType::Pattern, pattern_test);
    lex_assert!(" unit", TokenType::Unit, unit_test);
    lex_assert!(" let", TokenType::Let, let_test);
    lex_assert!(" as", TokenType::As, as_test);
    lex_assert!(" is", TokenType::Is, is_test);
    lex_assert!(" return", TokenType::Return, return_test);
    lex_assert!(" yield", TokenType::Yield, yield_test);
    lex_assert!(" pub", TokenType::Public, public_test);
    lex_assert!(" extern", TokenType::Extern, extern_test);

    lex_assert!(" @", TokenType::At, at_test);

    lex_assert!(" if", TokenType::If, if_test);
    lex_assert!(" else", TokenType::Else, else_test);

    lex_assert!(" _13292_293dh_238", TokenType::Identifier, ident_test_1);
    lex_assert!(" i1dw23", TokenType::Identifier, ident_test_2);

    lex_assert!(" 1", TokenType::Number, number_test_1);
    lex_assert!(" 1287321234567890", TokenType::Number, number_test_2);

    lex_assert!(" /", TokenType::Div, div_test);
    lex_assert!(" %", TokenType::Mod, mod_test);
    lex_assert!(" *", TokenType::Mul, mul_test);
    lex_assert!(" +", TokenType::Add, add_test);
    lex_assert!(" -", TokenType::Sub, sub_test);
    lex_assert!(" %%", TokenType::DMod, dmod_test);

    lex_assert!(" >", TokenType::GT, gt_test);
    lex_assert!(" <", TokenType::LT, lt_test);
    lex_assert!(" >=", TokenType::GE, ge_test);
    lex_assert!(" <=", TokenType::LE, le_test);
    lex_assert!(" ==", TokenType::EQ, eq_test);

    lex_assert!(" ->", TokenType::Arrow, arrow_test);
    lex_assert!(" =>", TokenType::FatArrow, fat_arrow_test);

    lex_assert!(" (", TokenType::LP, lp_test);
    lex_assert!(" )", TokenType::RP, rp_test);
    lex_assert!(" {", TokenType::LCP, lcp_test);
    lex_assert!(" }", TokenType::RCP, rcp_test);
    lex_assert!(" [", TokenType::LB, lb_test);
    lex_assert!(" ]", TokenType::RB, rb_test);
    lex_assert!(" ?", TokenType::Question, question_test);
    lex_assert!(" .", TokenType::Dot, dot_test);
    lex_assert!(" =", TokenType::Equals, equals_test);
    lex_assert!(" :", TokenType::Colon, color_test);
    lex_assert!(" ::", TokenType::DoubleColon, double_colon_test);
    lex_assert!(" ,", TokenType::Comma, comma_test);
    lex_assert!(" _", TokenType::Underscore, underscore_test);
}
