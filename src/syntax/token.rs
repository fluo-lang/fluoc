use smol_str::SmolStr;

pub type Str = SmolStr;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
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
