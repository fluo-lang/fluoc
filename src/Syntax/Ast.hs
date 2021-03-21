module Syntax.Ast where

import           Sources

data Ident = Ident String Span
  deriving (Eq, Show)
data Operator = Operator String Span
  deriving (Eq, Show)
data Namespace = Namespace [Ident] Span
  deriving (Eq, Show)

newtype Block = Block [Statement]
  deriving (Eq, Show)

data Declaration = Declaration Ident Type Span
  deriving (Eq, Show)

data Binding = Binding Ident [Pattern] Expr Span
  deriving (Eq, Show)

data BindingOrDec = BindingBOD Binding
                  | DeclarationBOD Declaration
                  deriving (Eq, Show)

data RecordItem = Union [Type]
                | Sum [Declaration]
                deriving (Eq, Show)

data Statement = BindingS Binding
               | DeclarationS Declaration
               | ImplS Ident Type [BindingOrDec] Span
               | TraitS Ident [Ident] [BindingOrDec] Span
               | RecordS Ident [Ident] [RecordItem] Span
               | ImportS Namespace (Maybe Ident) Span
               | FromImportS Namespace (Maybe [Ident])
               deriving (Eq, Show)

data Pattern = TupleP [Pattern] Span
             | BindP Ident
             | VariantP Namespace [Ident] Span
             | DropP Span
             | LiteralP Literal Span
             | CustomP Pattern Operator Pattern
             deriving (Eq, Show)

data Literal = IntegerL Int
             | FloatL Float
             deriving (Eq, Show)

data Type = Infer Span
          | Never Span
          | NamespaceType Namespace
          | TypeApplication Namespace [Type] Span
          deriving (Eq, Show)

data Expr = LiteralE Literal Span
          | BinOpE Expr  Expr Span
          | TupleE [Expr] Span
          | CondE (Expr, Expr) [(Expr, Expr)] (Expr, Expr)
          | LetInE Binding Expr Span
          | VariableE Namespace
          | FunctionAppE Expr Expr
          deriving (Eq, Show)

newtype Op = Op (String, Span)
  deriving (Eq, Show)
