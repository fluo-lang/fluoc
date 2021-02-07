use std::iter::{Iterator, Peekable};
use std::str::Chars;

use super::token::{Str, Token};
use crate::diagnostics::{ErrorPrelude::*, Failible, SourceId, Sources, Span, Spanned};

/// Lexer
pub struct Lexer<'s> {
    source_id: SourceId,
    chars: Peekable<Chars<'s>>,
    position: usize,
}

const EOF: char = '\0';

impl<'s> Iterator for Lexer<'s> {
    type Item = Failible<Spanned<Token>>;

    fn next(&mut self) -> Option<Self::Item> {
        Some(self.next_token())
    }
}

impl<'s> Lexer<'s> {
    /// Create a new lexer
    fn new(sources: &'s Sources, source_id: SourceId) -> Self {
        Self {
            chars: sources.get_source(&source_id).unwrap().chars().peekable(),
            source_id,
            position: 0,
        }
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
    fn next_token(&mut self) -> Failible<Spanned<Token>> {
        let s = self.position;
        let tok = match self.eat() {
            ':' => Token::Colon,
            '{' => Token::LCurly,
            '}' => Token::RCurly,
            '[' => Token::LBracket,
            ']' => Token::RBracket,
            '(' => Token::LParen,
            ')' => Token::RParen,

            c if Self::is_id_start(c) => self.eat_ident(c),
            c => {
                let span = Span::new(s, self.position, self.source_id);
                return Err(
                    Error::build(Level::Error, DiagnosticType::UnexpectedCharacter, span)
                        .annotation(Level::Error, format!("unexpected character `{}`", c), span),
                );
            }
        };
        let e = self.position;
        Ok(Span::new(s, e, self.source_id).spanned(tok))
    }

    fn eat_ident(&mut self, first: char) -> Token {
        let mut acc = String::with_capacity(1);
        acc.push(first);
        while Self::is_id_continue(self.peek()) {
            acc.push(self.eat());
        }
        match &acc[..] {
            "let" => Token::Let,
            "return" => Token::Return,
            "import" => Token::Import,
            "rec" => Token::Record,
            "fun" => Token::Function,
            "inst" => Token::Instance,
            "class" => Token::Typeclass,
            _ => Token::Ident(Str::new(acc)),
        }
    }

    #[inline]
    fn is_id_start(c: char) -> bool {
        c.is_ascii_alphabetic()
    }

    #[inline]
    fn is_id_continue(c: char) -> bool {
        c.is_ascii_alphanumeric()
    }
}

#[cfg(test)]
mod lexer_tests {
    use super::*;
    use crate::diagnostics::Spanned;
    use paste::paste;
    use std::ops::Deref;

    #[test]
    fn new_lexer() {
        let mut sources = Sources::new();
        let source_id = sources.add_source("10".to_string());
        Lexer::new(&sources, source_id);
    }

    macro_rules! next_token {
        (test $source: expr, $token: expr, $name: ident) => {
            paste! {
                #[test]
                fn [<$name _next>]() {
                    let mut sources = Sources::new();
                    let source_id = sources.add_source($source);
                    let mut lexer = Lexer::new(&sources, source_id);
                    let token = lexer.next().unwrap();
                    assert_eq!(
                        token
                            .as_ref()
                            .map(|t| <Spanned<Token> as Deref>::deref(t)),
                        Ok($token)
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
                            .map(|t| <Spanned<Token> as Deref>::deref(t)),
                        Ok($token)
                    );

                    // Should have the same value because peek
                    let token = lexer.peek().unwrap();
                    assert_eq!(
                        token
                            .as_ref()
                            .map(|t| <Spanned<Token> as Deref>::deref(t)),
                        Ok($token)
                    );
                }
            }
        };
    }

    next_token!(test "10".to_string(), &Token::Integer(Str::new("10")), lexer_integer);

    next_token!(test "{".to_string(), &Token::LCurly, lexer_lcurly);
    next_token!(test "}".to_string(), &Token::RCurly, lexer_rcurly);

    next_token!(test "[".to_string(), &Token::LBracket, lexer_lbracket);
    next_token!(test "]".to_string(), &Token::RBracket, lexer_rbracket);

    next_token!(test "(".to_string(), &Token::LParen, lexer_lparen);
    next_token!(test ")".to_string(), &Token::RParen, lexer_rparen);

    next_token!(test "let".to_string(), &Token::Let, lexer_let);
    next_token!(test "return".to_string(), &Token::Return, lexer_return);
    next_token!(test "import".to_string(), &Token::Import, lexer_import);
    next_token!(test "rec".to_string(), &Token::Record, lexer_record);
    next_token!(test "fun".to_string(), &Token::Function, lexer_function);
    next_token!(test "inst".to_string(), &Token::Instance, lexer_instance);
    next_token!(test "class".to_string(), &Token::Typeclass, lexer_typeclass);
}
