use smallvec::{smallvec, SmallVec};
use std::iter::Peekable;

use super::ast::*;
use super::{Lexer, Token, TokenKind};
use crate::common::Str;
use crate::diagnostics::prelude::*;

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    source_id: SourceId,
    current_span: Span,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            source_id: lexer.source_id(),
            lexer: lexer.peekable(),
            current_span: Span::dummy(),
        }
    }

    fn _peek(&mut self) -> &Failible<Token> {
        self.lexer.peek().unwrap()
    }

    fn _next(&mut self) -> Failible<Token> {
        let next = self.lexer.next().unwrap()?;
        self.current_span = next.span;
        Ok(next)
    }

    fn advance(&mut self) {
        let _ = self._next();
    }

    fn peek_sync(&mut self) -> Failible<&Token> {
        let tok = self._peek();
        match tok {
            Ok(val) => Ok(val),
            Err(_) => {
                let mut e = self._next().unwrap_err();
                e.extend(&mut self.synchronize());
                Err(e)
            }
        }
    }

    fn next_sync(&mut self) -> Failible<Token> {
        match self._next() {
            Ok(val) => Ok(val),
            Err(mut e) => {
                e.extend(&mut self.synchronize());
                Err(e)
            }
        }
    }

    fn expect(&mut self, token: TokenKind) -> Failible<Span> {
        let tok = self.next_sync()?;
        if token.is_same_variant(&tok.kind) {
            Ok(tok.span)
        } else {
            Err(Self::err(tok.span, &[token]))
        }
    }

    /// Calculate span from previous location and current location
    fn calc_span(&mut self, starting: Span) -> Span {
        Span::new(starting.s(), self.current_span.e(), self.source_id)
    }

    fn synchronize(&mut self) -> Diagnostics {
        let mut errs = SmallVec::new();
        loop {
            match self._next() {
                Ok(t) => {
                    if let TokenKind::Eof | TokenKind::Break = t.kind {
                        break;
                    }
                }
                Err(e) => errs.append(&mut e.into_inner()),
            }
        }

        errs.into()
    }

    fn err(span: Span, expected: &[TokenKind]) -> Diagnostics {
        let message = match expected.len() {
            0 => Cow::Borrowed("unexpected token"),
            1 => Cow::Owned(format!("expected {}", expected[0])),
            2 => Cow::Owned(format!("expected {} or {}", expected[0], expected[1])),
            len => Cow::Owned(format!(
                "expected one of {}, or {}",
                expected
                    .iter()
                    .take(len - 1)
                    .map(|t| t.display())
                    .collect::<Vec<&str>>()
                    .join(", "),
                expected[len - 1]
            )),
        };
        Diagnostic::build(Level::Error, DiagnosticType::Syntax, span)
            .annotation(Level::Error, message, span)
            .into()
    }

    pub fn statements(&mut self, pred: &dyn Fn(&TokenKind) -> bool) -> Failible<Vec<Statement>> {
        let mut root = Vec::new();
        let mut errors = SmallVec::<[Diagnostic; 1]>::new();

        loop {
            match self._peek().as_ref() {
                Ok(Token { kind, .. }) if pred(kind) => {
                    self.advance();
                    break;
                }
                Err(_) => return Err(self._next().unwrap_err()),
                _ => {}
            }

            match self.outer_statement() {
                Ok(stmt) => {
                    root.push(stmt);
                }
                Err(e) => {
                    let mut es = e.into_inner();
                    errors.append(&mut es);
                }
            };
        }

        if errors.is_empty() {
            Ok(root)
        } else {
            Err(errors.into())
        }
    }

    pub fn parse(&mut self) -> Failible<RootAst> {
        let root = self.statements(&|t| t.is_same_variant(&TokenKind::Eof))?;
        Ok(RootAst { stmts: root })
    }

    fn block(&mut self) -> Failible<Block> {
        let start = self.expect(TokenKind::LCurly)?;
        let root = self.statements(&|t| t.is_same_variant(&TokenKind::RCurly))?;
        Ok(Block {
            stmts: root,
            span: self.calc_span(start),
        })
    }

    fn outer_statement(&mut self) -> Failible<Statement> {
        let next = self.next_sync()?;
        match match next.kind {
            TokenKind::Function => self.function(),
            _ => return Err(Self::err(next.span, &[TokenKind::Function])),
        } {
            Ok(e) => Ok(e),
            Err(e) => {
                println!("{:?}", self._peek());
                self.synchronize();
                println!("{:?}", self._peek());
                Err(e)
            }
        }
    }

    fn function(&mut self) -> Failible<Statement> {
        let name = self.ident()?;
        self.expect(TokenKind::LParen)?;
        let arguments = self.function_args()?;
        self.expect(TokenKind::RParen)?;

        let return_ty = if let TokenKind::ColonColon = self.peek_sync()?.kind {
            self.advance();
            self.ty()?
        } else {
            Type::Infer
        };

        let block = self.block()?;

        Ok(Statement::FunctionDef(FunctionDef {
            span: self.calc_span(name.span),
            name,
            return_ty,
            arguments,
            block,
        }))
    }

    fn function_args(&mut self) -> Failible<FunctionArguments> {
        Ok(FunctionArguments {
            pos_args: vec![],
            kw_args: vec![],
        })
    }

    fn ty(&mut self) -> Failible<Type> {
        let next = self.peek_sync()?;
        match next.kind {
            TokenKind::Ident(_) => Ok(Type::Name(self.namespace()?)),
            TokenKind::Underscore => {
                self.advance();
                Ok(Type::Infer)
            }
            _ => Err(Self::err(
                next.span,
                &[TokenKind::Ident(Str::default()), TokenKind::Underscore],
            )),
        }
    }

    fn namespace(&mut self) -> Failible<Name> {
        let span = self.current_span;
        let id = self.next_sync()?;
        let mut items = if let TokenKind::Ident(content) = id.kind {
            smallvec![content]
        } else {
            return Err(Self::err(id.span, &[TokenKind::Ident(Str::default())]));
        };

        loop {
            if self.peek_sync()?.kind.is_same_variant(&TokenKind::Dot) {
                self.advance();
                let id = self.next_sync()?;
                if let TokenKind::Ident(content) = id.kind {
                    items.push(content)
                } else {
                    return Err(Self::err(id.span, &[TokenKind::Ident(Str::default())]));
                }
            } else {
                break;
            }
        }

        Ok(Name {
            items,
            span: self.calc_span(span),
        })
    }

    fn ident(&mut self) -> Failible<Ident> {
        let id = self.next_sync()?;
        if let TokenKind::Ident(content) = id.kind {
            Ok(Ident::new(content, id.span))
        } else {
            Err(Self::err(id.span, &[TokenKind::Ident(Str::default())]))
        }
    }
}

#[cfg(test)]
mod parser_tests {
    use super::*;
    use crate::diagnostics::Sources;

    #[test]
    fn err_0() {
        let err = Parser::err(Span::dummy(), &[]).into_inner()[0].annotations[0]
            .message
            .to_owned();
        assert_eq!(err, Cow::Borrowed("unexpected token"));
    }

    #[test]
    fn err_1() {
        let err = Parser::err(Span::dummy(), &[TokenKind::ColonColon]).into_inner()[0].annotations
            [0]
        .message
        .to_owned();
        assert_eq!(err, Cow::Borrowed("expected `::`"));
    }

    #[test]
    fn err_2() {
        let err = Parser::err(Span::dummy(), &[TokenKind::ColonColon, TokenKind::Let]).into_inner()
            [0]
        .annotations[0]
            .message
            .to_owned();
        assert_eq!(err, Cow::Borrowed("expected `::` or `let`"));
    }

    #[test]
    fn err_many() {
        let err = Parser::err(
            Span::dummy(),
            &[
                TokenKind::ColonColon,
                TokenKind::Let,
                TokenKind::RParen,
                TokenKind::Ident(Str::default()),
            ],
        )
        .into_inner()[0]
            .annotations[0]
            .message
            .to_owned();
        assert_eq!(
            err,
            Cow::Borrowed("expected one of `::`, `let`, `)`, or identifier")
        );
    }

    macro_rules! assert_ast {
        ($source: expr, $ast: expr, $fn: expr, $name: ident) => {
            #[test]
            fn $name() {
                let mut sources = Sources::new();
                let source_id = sources.add_source($source);
                let mut parser = Parser::new(Lexer::new(&sources, source_id));
                assert_eq!($fn(&mut parser), Ok($ast));
            }
        };
    }

    assert_ast!(
        "_123test._123t.a.b.c.d".to_string(),
        Name {
            items: smallvec![
                Str::from("_123test"),
                Str::from("_123t"),
                Str::from("a"),
                Str::from("b"),
                Str::from("c"),
                Str::from("d")
            ],
            span: Span::dummy()
        },
        Parser::namespace,
        namespace_many_items
    );

    assert_ast!(
        "_0test.test_more".to_string(),
        Name {
            items: smallvec![Str::from("_0test"), Str::from("test_more")],
            span: Span::dummy()
        },
        Parser::namespace,
        namespace_two_items
    );

    assert_ast!(
        "_0test".to_string(),
        Name {
            items: smallvec![Str::from("_0test")],
            span: Span::dummy()
        },
        Parser::namespace,
        namespace_one_item
    );

    assert_ast!(
        r#"fun test_fn() {}"#.to_string(),
        RootAst {
            stmts: vec![Statement::FunctionDef(FunctionDef {
                name: Ident::n("test_fn"),
                arguments: FunctionArguments {
                    kw_args: vec![],
                    pos_args: vec![],
                },
                return_ty: Type::Infer,
                block: Block {
                    stmts: Vec::new(),
                    span: Span::dummy(),
                },
                span: Span::dummy(),
            })]
        },
        Parser::parse,
        function_no_args_no_ty
    );

    #[test]
    fn sync_err() {
        let mut sources = Sources::new();
        let source_id = sources.add_source("fun\nfun\nfun".to_string());
        let mut parser = Parser::new(Lexer::new(&sources, source_id));
        assert_eq!(
            dbg!(parser.parse()).map_err(|e| e.into_inner().len()),
            Err(2)
        );
    }
}
