use std::{iter::Peekable, str::Chars};

use crate::common::{
    diagnostic::{Diagnostic, ErrorType, Failable, FileId, Span, Spanned, Tagged},
    interner::{Interner, StrId},
};

pub struct Lexer<'a> {
    stream: Peekable<Chars<'a>>,
    position: usize,
    file_id: FileId,
    interner: &'a mut Interner,
}

#[derive(Debug, PartialEq)]
pub enum Token {
    LParen,
    RParen,
    LAngle,
    RAngle,

    Equals,
    Colon,

    Fn,
    End,
    Type,
    Case,
    Do,
    Import,

    String(StrId),
    Operator(StrId),
    Atom(StrId),
    Ident(StrId),

    Eof,
}

const EOF: char = '\0';

impl<'a> Iterator for Lexer<'a> {
    type Item = Failable<Spanned<Token>>;
    fn next(&mut self) -> Option<Self::Item> {
        Some(self.next_token())
    }
}

impl<'a> Lexer<'a> {
    pub fn new(
        stream: Peekable<Chars<'a>>,
        file_id: FileId,
        interner: &'a mut Interner,
    ) -> Lexer<'a> {
        Self {
            stream,
            file_id,
            position: 0,
            interner,
        }
    }

    pub fn next_token(&mut self) -> Failable<Spanned<Token>> {
        let start_pos = self.position;
        let token = match self.eat() {
            '(' => Token::LParen,
            ')' => Token::RParen,
            '>' => Token::LAngle,
            '<' => Token::RAngle,
            ':' => {
                let next = self.peek_char();
                if Self::is_id_start(next) {
                    self.eat();
                    let s = self.consume_while(next, Self::is_id_continue);
                    Token::Atom(self.interner.intern(&s))
                } else {
                    let span = Span::new(start_pos, start_pos + 1, self.file_id);
                    return Err(Diagnostic::new(
                        ErrorType::LexError,
                        "valid identitfier must follow `:`",
                        span,
                    ));
                }
            }
            c if Self::is_id_start(c) => {
                let s = self.consume_while(c, Self::is_id_continue);
                Token::Ident(self.interner.intern(&s))
            }
            c if Self::is_whitespace(c) => {
                return self.next_token();
            }
            '\0' => Token::Eof,
            _ => {
                let span = Span::new(start_pos, start_pos + 1, self.file_id);
                return Err(
                    Diagnostic::new(ErrorType::LexError, "unrecognized character", span)
                        .with_annot_no_msg(span),
                );
            }
        };
        let end_pos = self.position;
        Ok(Tagged(token, Span::new(start_pos, end_pos, self.file_id)))
    }

    fn is_operator(c: char) -> bool {
        match c {
            '!' | '@' | '$' | '%' | '^' | '&' | '*' | '-' | '+' | '=' | '>' | '<' | '.' | '?'
            | '/' => true,
            _ => false,
        }
    }

    #[inline]
    /// Go forward one character in the character stream
    fn eat(&mut self) -> char {
        self.position += 1;
        self.stream.next().unwrap_or(EOF)
    }

    #[inline]
    /// Go forward one character without peeking
    fn peek_char(&mut self) -> char {
        *self.stream.peek().unwrap_or(&EOF)
    }

    #[inline]
    fn is_id_start(c: char) -> bool {
        c.is_ascii_alphabetic() || c == '_'
    }

    #[inline]
    fn is_id_continue(c: char) -> bool {
        c.is_ascii_alphanumeric() || c == '_'
    }

    #[inline]
    fn is_eof(&mut self) -> bool {
        self.peek_char() == EOF
    }

    #[inline]
    fn is_whitespace(c: char) -> bool {
        c == ' ' || c == '\n' || c == '\t'
    }

    #[inline]
    fn consume_while(&mut self, first: char, predicate: impl Fn(char) -> bool) -> String {
        let mut acc = String::with_capacity(1);
        acc.push(first);
        while predicate(self.peek_char()) && !self.is_eof() {
            acc.push(self.eat());
        }
        acc
    }

    fn consume_until(&mut self, predicate: impl Fn(char) -> bool) -> String {
        let mut acc = String::with_capacity(1);
        while predicate(self.peek_char()) && !self.is_eof() {
            acc.push(self.eat());
        }
        acc
    }
}

#[cfg(test)]
mod test_lexer {
    use super::*;
    use std::collections::HashMap;

    macro_rules! assert_tokens {
        ($input_str: expr, $token_slice: expr, $test_name: ident) => {
            #[test]
            fn $test_name() -> Failable<()> {
                let stream = $input_str.chars().peekable();
                let mut interner = Interner::default();
                let mut lexer = Lexer::new(stream, FileId::default(), &mut interner);
                let mut next_tok = lexer.next().unwrap()?.0;
                let mut collected_tokens = Vec::new();
                while next_tok != Token::Eof {
                    collected_tokens.push(next_tok);
                    next_tok = lexer.next().unwrap()?.0;
                }
                assert_eq!(collected_tokens, $token_slice);
                Ok(())
            }
        };
    }

    assert_tokens!("", [], empty);
    assert_tokens!("()", [Token::LParen, Token::RParen], paren);
    assert_tokens!(" ( \t) ", [Token::LParen, Token::RParen], paren_with_space);

    macro_rules! match_tokens {
        ($test_name: ident, $input_str: expr, {$($k:expr => $v:expr),*}, $($matches: pat),*) => {
            #[test]
            fn $test_name() -> Failable<()> {
                let stream = $input_str.chars().peekable();
                let mut interner = Interner::default();
                let mut lexer = Lexer::new(stream, FileId::default(), &mut interner);

                let mut token = 0;
                $({
                    let next_tok = lexer.next().unwrap()?.0;
                    if let $matches = next_tok {
                        token += 1;
                    } else {
                        panic!("Failed at token {}. Found token: {:?}.", token, next_tok)
                    }
                })*
                let next_tok = lexer.next().unwrap()?.0;
                assert_eq!(next_tok, Token::Eof);

                let expectations: HashMap<usize, &str> = HashMap::from([$(($k, $v),)*]);
                for (k, v) in expectations {
                    let s = interner.lookup(StrId(k));
                    assert_eq!(s, v, "Index {} was `{}`, but expected it to be `{}`", k, s, v);
                }
                Ok(())
            }
        };
    }

    match_tokens!(
        atom_test,
        "(:testing ok)",
        {0 => "testing", 1 => "ok"},
        Token::LParen,
        Token::Atom(StrId(0)),
        Token::Ident(StrId(1)),
        Token::RParen
    );

    match_tokens!(
        ident_with_space,
        "abc efghijklmno_pqrstuvwxyz _QWERTYUIOP ASDFGHJKL ZXCVBNM",
        {
            0 => "abc",
            1 => "efghijklmno_pqrstuvwxyz",
            2 => "_QWERTYUIOP",
            3 => "ASDFGHJKL",
            4 => "ZXCVBNM"
        },
        Token::Ident(StrId(0)),
        Token::Ident(StrId(1)),
        Token::Ident(StrId(2)),
        Token::Ident(StrId(3)),
        Token::Ident(StrId(4))
    );
}
