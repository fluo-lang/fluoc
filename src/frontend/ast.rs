struct FileId(usize);
struct Span {
    s: usize,
    e: usize,
    fid: FileId,
}
struct Spanned<T>(T, Span);
enum Statement {
    Expression(Spanned<Expr>),
}

enum Expr {}
