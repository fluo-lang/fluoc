use std::iter::{Iterator, Peekable};
use std::str::Chars;

use super::token::{Token, TokenKind};

use crate::common::Str;
use crate::diagnostics::{prelude::*, Failible, SourceId, Sources, Span};

/// Lexer
pub struct Lexer<'s> {
    source_id: SourceId,
    chars: Peekable<Chars<'s>>,
    position: usize,
    emit_break: bool,
}

const EOF: char = '\0';

impl<'s> Iterator for Lexer<'s> {
    type Item = Failible<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        Some(self.next_token())
    }
}

impl<'s> Lexer<'s> {
    /// Create a new lexer
    pub fn new(sources: &'s Sources, source_id: SourceId) -> Self {
        Self {
            chars: sources.get_source(&source_id).unwrap().chars().peekable(),
            source_id,
            position: 0,
            emit_break: false,
        }
    }

    /// Get source_id of lexer
    pub fn source_id(&self) -> SourceId {
        self.source_id
    }

    #[inline]
    /// Go forward one character in the character stream
    fn eat(&mut self) -> char {
        self.position += 1;
        self.chars.next().unwrap_or(EOF)
    }

    #[inline]
    /// Go forward one character without peeking
    fn peek(&mut self) -> char {
        *self.chars.peek().unwrap_or(&EOF)
    }

    /// Get the next token in the input stream. Consumes
    /// the token, and moves onto the next.
    fn next_token(&mut self) -> Failible<Token> {
        let s = self.position;
        let kind = match self.eat() {
            '{' => TokenKind::LCurly,
            '}' => TokenKind::RCurly,
            '[' => TokenKind::LBracket,
            ']' => TokenKind::RBracket,
            '(' => TokenKind::LParen,
            ')' => TokenKind::RParen,

            '_' if !Self::is_id_continue(self.peek()) => TokenKind::Underscore,
            '"' => self.eat_string()?,
            '#' => {
                self.eat_comment();
                return self.next_token();
            }
            c if c == EOF => TokenKind::Eof,
            c if Self::is_newline(c) => {
                if self.emit_break {
                    TokenKind::Break
                } else {
                    return self.next_token();
                }
            }
            c if Self::is_whitespace(c) => {
                self.consume_while(c, Self::is_whitespace);
                return self.next_token();
            }
            c if Self::is_id_start(c) => self.eat_ident(c),
            c if Self::is_operator(c) => self.eat_operator(c),
            c if c.is_ascii_digit() => self.eat_number(c),
            c => {
                let span = Span::new(s, self.position, self.source_id);
                return Err(Diagnostic::build(
                    Level::Error,
                    DiagnosticType::UnexpectedCharacter,
                    span,
                )
                .annotation(
                    Level::Error,
                    Cow::Owned(format!("unexpected character `{}`", c)),
                    span,
                )
                .into());
            }
        };
        let e = self.position;
        self.emit_break = Self::should_emit_break(&kind);
        Ok(Token {
            kind,
            span: Span::new(s, e, self.source_id),
        })
    }

    #[inline]
    fn is_operator(c: char) -> bool {
        match c {
            '+' | '*' | '-' | '/' | '<' | '>' | '|' | ':' | '$' | '^' | '@' | '!' | '~' | '%'
            | '.' | '&' | '=' => true,
            _ => false,
        }
    }

    fn consume_while(&mut self, first: char, predicate: impl Fn(char) -> bool) -> String {
        let mut acc = String::with_capacity(1);
        acc.push(first);
        while predicate(self.peek()) {
            acc.push(self.eat());
        }
        acc
    }

    fn eat_comment(&mut self) {
        self.consume_while(' ', |c| !Self::is_newline(c));
    }

    fn eat_number(&mut self, first: char) -> TokenKind {
        let start = self.consume_while(first, |c| c.is_ascii_digit());
        TokenKind::Integer(Str::new(start))
    }

    fn eat_operator(&mut self, first: char) -> TokenKind {
        let acc = self.consume_while(first, Self::is_operator);
        match &acc[..] {
            "..." => TokenKind::DotDotDot,
            "." => TokenKind::Dot,
            ":" => TokenKind::Colon,
            "::" => TokenKind::ColonColon,
            "->" => TokenKind::Arrow,
            "=" => TokenKind::Equals,
            "|" => TokenKind::Pipe,
            "=:" => TokenKind::EqColon,
            _ => TokenKind::Symbol(Str::new(acc)),
        }
    }

    fn eat_ident(&mut self, first: char) -> TokenKind {
        let acc = self.consume_while(first, Self::is_id_continue);
        match &acc[..] {
            "let" => TokenKind::Let,
            "return" => TokenKind::Return,
            "import" => TokenKind::Import,
            "rec" => TokenKind::Record,
            "fun" => TokenKind::Function,
            "inst" => TokenKind::Instance,
            "class" => TokenKind::Typeclass,
            _ => TokenKind::Ident(Str::new(acc)),
        }
    }

    fn eat_string(&mut self) -> Result<TokenKind, Diagnostic> {
        let mut acc = String::new();
        loop {
            let next = self.eat();
            if next == '\\' {
                match self.peek() {
                    '\\' => {
                        // Escaped backslash
                        self.eat();
                        acc.push('\\')
                    }
                    '"' => {
                        // Escaped qoute
                        self.eat();
                        acc.push('"')
                    }
                    'n' => {
                        // Escaped newline
                        self.eat();
                        acc.push('\n')
                    }
                    c => {
                        let span = Span::new(self.position, self.position + 1, self.source_id);
                        return Err(Diagnostic::build(
                            Level::Error,
                            DiagnosticType::InvalidEscapeSequence,
                            span,
                        )
                        .annotation(
                            Level::Error,
                            Cow::Owned(format!("invalid escape `\\{}`", c)),
                            span,
                        ));
                    }
                }
            } else if next == '"' {
                break;
            } else {
                acc.push(next);
            }
        }
        Ok(TokenKind::String(Str::new(acc)))
    }

    fn should_emit_break(tok: &TokenKind) -> bool {
        match tok {
            TokenKind::RParen
            | TokenKind::RCurly
            | TokenKind::RBracket
            | TokenKind::Integer(_)
            | TokenKind::Float(_)
            | TokenKind::String(_)
            | TokenKind::Ident(_)
            | TokenKind::Symbol(_) => true,
            _ => false,
        }
    }

    fn is_newline(c: char) -> bool {
        match c {
            '\n' // Normal line break
                |'\u{0085}' // Next line from latin1
                | '\u{2028}' // Line separator
                | '\u{2029}' // Paragraph separator
                => true,

            _ => false
        }
    }

    fn is_whitespace(c: char) -> bool {
        match c {
            '\u{0009}' // \t
                | '\u{000B}' // vertical tab
                | '\u{000C}' // form feed
                | '\u{000D}' // \r
                | '\u{0020}' // space
                => true,

            _ => false
        }
    }

    #[inline]
    fn is_id_start(c: char) -> bool {
        c.is_ascii_alphabetic() || c == '_'
    }

    #[inline]
    fn is_id_continue(c: char) -> bool {
        c.is_ascii_alphanumeric() || c == '_'
    }
}

#[cfg(test)]
mod lexer_tests {
    use super::*;
    use paste::paste;

    #[test]
    fn new_lexer() {
        let mut sources = Sources::new();
        let source_id = sources.add_source("10".to_string());
        Lexer::new(&sources, source_id);
    }

    #[test]
    fn consume_while() {
        let mut sources = Sources::new();
        let source_id = sources.add_source("aaaab".to_string());
        let mut lexer = Lexer::new(&sources, source_id);
        assert_eq!(&lexer.consume_while('a', |c| c == 'a')[..], "aaaaa");
    }

    macro_rules! next_token {
        (test $source: expr, $err: expr, $name: ident) => {
            next_token!(inner $source, $err, $name);

            paste! {
                next_token!(inner format!("  {}", $source), $err, [<$name _whitespace>]);
            }
        };
        (inner $source: expr, $token: expr, $name: ident) => {
            paste! {
                #[test]
                fn [<$name _next>]() {
                    let mut sources = Sources::new();
                    let source_id = sources.add_source($source);
                    let mut lexer = Lexer::new(&sources, source_id);
                    let token = lexer.next().unwrap();
                    assert_eq!(
                        token.as_ref().map(|t| &t.kind),
                        Ok(&$token)
                    );
                }

                #[test]
                fn [<$name _peek>]() {
                    let mut sources = Sources::new();
                    let source_id = sources.add_source($source);
                    let mut lexer = Lexer::new(&sources, source_id).peekable();
                    let token = lexer.peek().unwrap();
                    assert_eq!(
                        token.as_ref().map(|t| &t.kind),
                        Ok(&$token)
                    );

                    // Should have the same value because peek
                    let token = lexer.peek().unwrap();
                    assert_eq!(
                        token.as_ref().map(|t| &t.kind),
                        Ok(&$token)
                    );
                }
            }
        };
    }

    macro_rules! token_error {
        (test $source: expr, $err: expr, $name: ident) => {
            paste! {
                #[test]
                fn [<$name _next>]() {
                    let mut sources = Sources::new();
                    let source_id = sources.add_source($source);
                    let mut lexer = Lexer::new(&sources, source_id);
                    let token = lexer.next().unwrap();
                    assert_eq!(
                        token
                            .map_err(|e| e.inner()[0].ty),
                        Err($err)
                    );
                }

                #[test]
                fn [<$name _peek>]() {
                    let mut sources = Sources::new();
                    let source_id = sources.add_source($source);
                    let mut lexer = Lexer::new(&sources, source_id).peekable();
                    let token = lexer.peek().unwrap();

                    assert_eq!(
                        token
                            .as_ref()
                            .map_err(|e| e.inner()[0].ty),
                        Err($err)
                    );

                    // Should have the same value because peek
                    let token = lexer.peek().unwrap();
                    assert_eq!(
                        token
                            .as_ref()
                            .map_err(|e| e.inner()[0].ty),
                        Err($err) as Result<&Token, DiagnosticType>
                    );
                }
            }
        };
    }

    token_error!(test "`".to_string(), DiagnosticType::UnexpectedCharacter, lexer_dot_dot_fail);
    token_error!(test r#""\@""#.to_string(), DiagnosticType::InvalidEscapeSequence, lexer_invalid_escape);

    next_token!(test "\n".to_string(), TokenKind::Eof, lexer_newline_eof);

    next_token!(test "10".to_string(), TokenKind::Integer(Str::new("10")), lexer_integer);
    next_token!(test "0123456789".to_string(), TokenKind::Integer(Str::new("0123456789")), lexer_long_integer);

    next_token!(test "# Comment test\n0123456789".to_string(), TokenKind::Integer(Str::new("0123456789")), lexer_comment);

    next_token!(test r#""awdsad""#.to_string(), TokenKind::String(Str::new("awdsad")), lexer_string);
    next_token!(test r#""awd\"sad""#.to_string(), TokenKind::String(Str::new(r#"awd"sad"#)), lexer_string_escaped_qoute);
    next_token!(test r#""awdsad\\""#.to_string(), TokenKind::String(Str::new(r#"awdsad\"#)), lexer_string_escaped_backslash);
    next_token!(test r#""\n""#.to_string(), TokenKind::String(Str::new("\n")), lexer_string_escaped_newline);

    next_token!(test "{".to_string(), TokenKind::LCurly, lexer_lcurly);
    next_token!(test "}".to_string(), TokenKind::RCurly, lexer_rcurly);

    next_token!(test "[".to_string(), TokenKind::LBracket, lexer_lbracket);
    next_token!(test "]".to_string(), TokenKind::RBracket, lexer_rbracket);

    next_token!(test "(".to_string(), TokenKind::LParen, lexer_lparen);
    next_token!(test ")".to_string(), TokenKind::RParen, lexer_rparen);

    next_token!(test ":".to_string(), TokenKind::Colon, lexer_colon);
    next_token!(test "=".to_string(), TokenKind::Equals, lexer_equals);
    next_token!(test "=:".to_string(), TokenKind::EqColon, lexer_eq_colon);
    next_token!(test ".".to_string(), TokenKind::Dot, lexer_dot);
    next_token!(test "...".to_string(), TokenKind::DotDotDot, lexer_dot_dot_dot);
    next_token!(test "->".to_string(), TokenKind::Arrow, lexer_arrow);
    next_token!(test "_".to_string(), TokenKind::Underscore, lexer_underscore);

    next_token!(test "!@!".to_string(), TokenKind::Symbol(Str::new("!@!")), lexer_custom_operator);
    next_token!(test "+*-/<>|:$^^@!~%.&=".to_string(), TokenKind::Symbol(Str::new("+*-/<>|:$^^@!~%.&=")), lexer_long_operator);

    next_token!(test "_wad".to_string(), TokenKind::Ident(Str::new("_wad")), lexer_ident_underscore_start);
    next_token!(test "awd_123".to_string(), TokenKind::Ident(Str::new("awd_123")), lexer_ident_number);
    next_token!(test "awd".to_string(), TokenKind::Ident(Str::new("awd")), lexer_ident_alphabetic);

    next_token!(test "let".to_string(), TokenKind::Let, lexer_let);
    next_token!(test "return".to_string(), TokenKind::Return, lexer_return);
    next_token!(test "import".to_string(), TokenKind::Import, lexer_import);
    next_token!(test "rec".to_string(), TokenKind::Record, lexer_record);
    next_token!(test "fun".to_string(), TokenKind::Function, lexer_function);
    next_token!(test "inst".to_string(), TokenKind::Instance, lexer_instance);
    next_token!(test "class".to_string(), TokenKind::Typeclass, lexer_typeclass);

    macro_rules! nth_token {
        (test $source: expr, $n: expr, $token: expr, $name: ident) => {
            #[test]
            fn $name() {
                let mut sources = Sources::new();
                let source_id = sources.add_source($source);
                let mut lexer = Lexer::new(&sources, source_id);
                let token = lexer.nth($n).unwrap();
                assert_eq!(token.map(|t| t.kind), Ok($token));
            }
        };
    }

    nth_token!(test "test\n".to_string(), 1, TokenKind::Break, lexer_break_after_id);
    nth_token!(test "10\n".to_string(), 1, TokenKind::Break, lexer_break_after_num);
    nth_token!(test ")\n".to_string(), 1, TokenKind::Break, lexer_break_after_rparen);
    nth_token!(test "]\n".to_string(), 1, TokenKind::Break, lexer_break_after_rbracket);
    nth_token!(test "}\n".to_string(), 1, TokenKind::Break, lexer_break_after_rcurly);
    nth_token!(test "!@!\n".to_string(), 1, TokenKind::Break, lexer_break_after_symbol);
    nth_token!(test "\"test\"\n".to_string(), 1, TokenKind::Break, lexer_break_after_string);

    nth_token!(test "(\n".to_string(), 1, TokenKind::Eof, lexer_no_break_after_lparen);
    nth_token!(test "[\n".to_string(), 1, TokenKind::Eof, lexer_no_break_after_lbracket);
    nth_token!(test "{\n".to_string(), 1, TokenKind::Eof, lexer_no_break_after_lcurly);
}
