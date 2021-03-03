module Syntax.Ast where

import qualified Data.Text                     as T
import           Sources

newtype Ident = Ident (T.Text, Span)
newtype Namespace = Namespace (T.Text, Span)

newtype Block = Block [Statement]

data Statement = FunDecl Ident Arguments Type Span
               | VarDecl Expr Expr Type Span

newtype Arguments = Arguments
                      (
                        ([(Ident, Type)],
                        [(Ident, Type, Maybe Expr)]),
                        Span
                      )

data Type = Infer | Never | NamespaceType Namespace

data Expr = Number T.Text Span
          | Float T.Text Span
          | FunctionCall Namespace [Expr] [(Ident, Expr)] Span
          | BlockExpr Block Span
          | StringExpr T.Text Span
          | InfixOp Op Expr Expr Span
          | PrefixOp Op Expr Span
          | PostfixOp Op Expr Span

newtype Op = Op (T.Text, Span)
