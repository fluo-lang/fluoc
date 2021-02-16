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
