module Syntax.Ast where

import           Sources

data Ident = Ident String Span
  deriving (Eq, Show)
data Namespace = Namespace [Ident] Span
  deriving (Eq, Show)

newtype Block = Block [Statement] deriving (Eq, Show)

data Statement = FunDecl Ident Arguments Type Block Span
               | VarDecl Expr Expr Type Span deriving (Eq, Show)

data Arguments = Arguments [(Ident, Type)]
                           [(Ident, Type, Maybe Expr)]
                           (Ident, Type)
                           Span
  deriving (Eq, Show)

data Type = Infer | Never | NamespaceType Namespace deriving (Eq, Show)

data Expr = Number String Span
          | Float String Span
          | FunctionCall Namespace [Expr] [(Ident, Expr)] Span
          | BlockExpr Block Span
          | StringExpr String Span
          | InfixOp Op Expr Expr Span
          | PrefixOp Op Expr Span
          | PostfixOp Op Expr Span deriving (Eq, Show)

newtype Op = Op (String, Span) deriving (Eq, Show)
