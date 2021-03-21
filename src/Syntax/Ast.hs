module Syntax.Ast where

import           Sources

data Ident = Ident String Span
  deriving (Eq, Show)
data Operator = Operator String Span
  deriving (Eq, Show)
data Namespace = Namespace [Ident] Span
  deriving (Eq, Show)

newtype Block = Block [Statement] deriving (Eq, Show)

data Statement = DeclarationS Ident Type Span
               | BindingS Ident [Pattern] Expr Span deriving (Eq, Show)

data Pattern = TupleP [Pattern] Span
             | BindP Ident
             | VariantP Namespace [Ident] Span
             | DropP Span
             | LiteralP Literal Span
             | CustomP Pattern Operator Pattern deriving (Eq, Show)

data Literal = IntegerL Int
             | FloatL Float deriving (Eq, Show)

data Type = Infer Span
          | Never Span
          | NamespaceType Namespace
          | TypeApplication Namespace [Type] Span deriving (Eq, Show)

data Expr = LiteralE Span
          | BinOpE Expr Operator Expr
  deriving (Eq, Show)

newtype Op = Op (String, Span) deriving (Eq, Show)
