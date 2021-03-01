use std::fmt;

use crate::common::Str;
use crate::diagnostics::Span;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TokenKind {
    /// E.g. `123`
    Integer(Str),
    /// E.g. `123.234`
    Float(Str),
    /// E.g. `"string foo bar"`
    String(Str),
    /// E.g. `foo`
    Ident(Str),
    /// E.g. `+|`, `+`, `@!`
    Symbol(Str),

    /// `let`
    Let,
    /// `return`
    Return,
    /// `import`
    Import,
    /// `rec`
    Record,
    /// `fun`
    Function,
    /// `inst`
    Instance,
    /// `class`
    Typeclass,

    /// `:`
    Colon,
    /// `::`
    ColonColon,
    /// `=`
    Equals,
    /// `.`
    Dot,
    /// `->`
    Arrow,
    /// `|`
    Pipe,
    /// `_`
    Underscore,
    /// `=:`
    EqColon,
    /// `...`
    DotDotDot,

    /// `(`
    LParen,
    /// `)`
    RParen,
    /// `[`
    LBracket,
    /// `]`
    RBracket,
    /// `{`
    LCurly,
    /// `}`
    RCurly,

    /// `\n`
    Break,

    /// End of file
    Eof,
}

macro_rules! get_token {
    ($t: expr) => {
        match $t {
            Self::Integer(_) => "integer literal",
            Self::Float(_) => "float literal",
            Self::String(_) => "string literal",
            Self::Ident(_) => "identifier",
            Self::Symbol(_) => "operator",

            Self::Let => "`let`",
            Self::Return => "`return`",
            Self::Import => "`import`",
            Self::Record => "`rec`",
            Self::Instance => "`inst`",
            Self::Typeclass => "`class`",
            Self::Function => "`fun`",

            Self::Colon => "`:`",
            Self::ColonColon => "`::`",
            Self::Equals => "`=`",
            Self::Dot => "`.`",
            Self::Arrow => "`=>`",
            Self::Pipe => "`|`",
            Self::Underscore => "`_`",
            Self::EqColon => "`=:`",
            Self::DotDotDot => "`...`",
            Self::LParen => "`(`",
            Self::RParen => "`)`",
            Self::LBracket => "`[`",
            Self::RBracket => "`]`",
            Self::LCurly => "`{`",
            Self::RCurly => "`}`",

            Self::Eof => "end of file",
            Self::Break => "line break",
        }
    };
}

impl TokenKind {
    pub fn display(&self) -> &'static str {
        get_token!(self)
    }

    pub fn is_same_variant(&self, other: &Self) -> bool {
        std::mem::discriminant(self) == std::mem::discriminant(other)
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.display())
    }
}

#[cfg(test)]
mod token_tests {
    use super::*;

    #[test]
    fn is_same_variant() {
        assert!(TokenKind::Ident(Str::default()).is_same_variant(&TokenKind::Ident(Str::default())));
        assert!(!TokenKind::Ident(Str::default()).is_same_variant(&TokenKind::ColonColon));
    }

    #[test]
    fn token() {
        assert_eq!(
            Token::new(TokenKind::Let, Span::dummy()),
            Token {
                kind: TokenKind::Let,
                span: Span::dummy()
            }
        );

        assert_ne!(
            Token::new(TokenKind::Let, Span::dummy()),
            Token {
                kind: TokenKind::Return,
                span: Span::dummy()
            }
        );
    }
}
