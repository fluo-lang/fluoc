use std::{iter::Peekable, str::Chars};

use crate::common::{
    diagnostic::{Diagnostic, ErrorType, Failable, FileId, Span, Spanned},
    interner::StrId,
};

pub struct Lexer<'a> {
    stream: Peekable<Chars<'a>>,
    position: usize,
    file_id: FileId,
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
    pub fn new(stream: Peekable<Chars<'a>>, file_id: FileId) -> Lexer<'a> {
        Self {
            stream,
            file_id,
            position: 0,
        }
    }

    pub fn next_token(&mut self) -> Failable<Spanned<Token>> {
        let start_pos = self.position;
        let token = match self.eat() {
            '(' => Token::LParen,
            ')' => Token::RParen,
            '\0' => Token::Eof,
            _ => {
                let span = Span::new(start_pos, start_pos + 1, self.file_id);
                return Err(Diagnostic::new(
                    ErrorType::LexError,
                    "unrecognized character",
                    span,
                ).with_annot_no_msg(span));
            }
        };
        let end_pos = self.position;
        Ok(Spanned(token, Span::new(start_pos, end_pos, self.file_id)))
    }

    fn is_operator(c: char) -> bool {
        match c {
            '!' | '@' | '$' | '%' | '^' | '&' | '*' | '-' | '+' | '=' | '>' | '<' | '.' | '?' | '/' => true,
            _ => false
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

    macro_rules! assert_tokens {
        ($input_str: expr, $token_slice: expr, $test_name: ident) => {
            #[test]
            fn $test_name() -> Failable<()> {
                let stream = $input_str.chars().peekable();
                let mut lexer = Lexer::new(stream, FileId::default());
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
}
