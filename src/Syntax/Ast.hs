module Syntax.Ast where

import           Sources

data Ident = Ident String Span
           | OpId Operator
           deriving (Eq, Show)
data Operator = Operator String Span
  deriving (Eq, Show)
data Namespace = Namespace [Ident] Span
  deriving (Eq, Show)

data Declaration = Declaration Ident Type Span
  deriving (Eq, Show)
data Binding = Binding (Maybe Ident) [Pattern] Expr Span
  deriving (Eq, Show)
data BindingOrDec = BindingBOD Binding
                  | DeclarationBOD Declaration
data RecordItem = Union [Type] Span
                | Sum [Declaration] Span
                deriving (Eq, Show)

data Statement = BindingS [Binding] Span
               | DeclarationS Declaration Span
               | ImplS Ident Type [Statement] Span
               | TraitS Ident [Ident] [Statement] Span
               | RecordS Ident [Ident] [RecordItem] Span
               | ImportS Namespace (Maybe Ident) Span
               | FromImportS Namespace (Maybe [Ident]) Span
               deriving (Eq, Show)

data Pattern = TupleP [Pattern] Span
             | BindP Ident Span
             | VariantP Namespace Pattern Span
             | DropP Span
             | LiteralP Literal Span
             | OperatorP (Oped Pattern) Span
             deriving (Eq, Show)

data Literal = IntegerL Integer Span
             | FloatL Double Span
             | StringL String Span
             deriving (Eq, Show)

data Type = Infer Span
          | Never Span
          | NamespaceType Namespace Span
          | OperatorType (Oped Type) Span
          | TupleType [Type] Span
          | PolyType String Span
          deriving (Eq, Show)

data Expr = LiteralE Literal Span
          | OperatorE (Oped Expr) Span
          | TupleE [Expr] Span
          | CondE (Expr, Expr) [(Expr, Expr)] Expr Span
          | LetInE [Binding] Expr Span
          | VariableE Namespace Span
          | LambdaE [Pattern] Expr Span
          | GroupedE Expr Span
          | MatchE Expr [MatchBranch] Span
          deriving (Eq, Show)

data MatchBranch = MatchBranch Pattern Expr Span
  deriving (Eq, Show)

data Oped a = BinOp Operator a a
            | PreOp Operator a
            | PostOp Operator a
            | Grouped a
            deriving (Eq, Show)

instance Spanned Ident where
  getSpan (Ident _ s) = s
  getSpan (OpId op  ) = getSpan op

instance Spanned Operator where
  getSpan (Operator _ s) = s

instance Spanned Namespace where
  getSpan (Namespace _ s) = s

instance Spanned Declaration where
  getSpan (Declaration _ _ s) = s

instance Spanned Binding where
  getSpan (Binding _ _ _ s) = s

instance Spanned BindingOrDec where
  getSpan (BindingBOD     b) = getSpan b
  getSpan (DeclarationBOD d) = getSpan d

instance Spanned RecordItem where
  getSpan (Union _ s) = s
  getSpan (Sum   _ s) = s

instance Spanned Statement where
  getSpan (BindingS     _ s ) = s
  getSpan (DeclarationS _ s ) = s
  getSpan (ImplS   _ _ _ s  ) = s
  getSpan (TraitS  _ _ _ s  ) = s
  getSpan (RecordS _ _ _ s  ) = s
  getSpan (ImportS     _ _ s) = s
  getSpan (FromImportS _ _ s) = s

instance Spanned Pattern where
  getSpan (TupleP _ s    ) = s
  getSpan (BindP  _ s    ) = s
  getSpan (VariantP _ _ s) = s
  getSpan (DropP s       ) = s
  getSpan (LiteralP  _ s ) = s
  getSpan (OperatorP _ s ) = s

instance Spanned Literal where
  getSpan (IntegerL _ s) = s
  getSpan (FloatL   _ s) = s
  getSpan (StringL  _ s) = s

instance Spanned Type where
  getSpan (Infer s          ) = s
  getSpan (Never s          ) = s
  getSpan (NamespaceType _ s) = s
  getSpan (OperatorType  _ s) = s
  getSpan (TupleType     _ s) = s

instance Spanned Expr where
  getSpan (LiteralE  _ s) = s
  getSpan (OperatorE _ s) = s
  getSpan (TupleE    _ s) = s
  getSpan (CondE _ _ _ s) = s
  getSpan (LetInE _ _ s ) = s
  getSpan (VariableE _ s) = s
  getSpan (LambdaE _ _ s) = s
  getSpan (GroupedE _ s ) = s
