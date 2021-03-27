module Syntax.Ast where

import           Sources

data Ident = Ident String Span
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
             | CustomP Pattern Operator Pattern Span
             deriving (Eq, Show)

data Literal = IntegerL Integer Span
             | FloatL Double Span
             | StringL String Span
             deriving (Eq, Show)

data Type = Infer Span
          | Never Span
          | NamespaceType Namespace Span
          | BinOpType Type Operator Type Span
          | TupleType [Type] Span
          deriving (Eq, Show)

data Expr = LiteralE Literal Span
          | BinOpE Expr Operator Expr Span
          | TupleE [Expr] Span
          | CondE (Expr, Expr) [(Expr, Expr)] Expr Span
          | LetInE [Binding] Expr Span
          | VariableE Namespace Span
          | LambdaE [Ident] Expr Span
          | GroupedE Expr Span
          deriving (Eq, Show)

instance Spanned Ident where
  getSpan (Ident _ s) = s
  setSpan newSp (Ident s _) = Ident s newSp
  
instance Spanned Operator where
  getSpan (Operator _ s) = s
  setSpan newSp (Operator s _) = Operator s newSp

instance Spanned Namespace where
  getSpan (Namespace _ s) = s
  setSpan newSp (Namespace s _) = Namespace s newSp

instance Spanned Declaration where
  getSpan (Declaration _ _ s) = s
  setSpan newSp (Declaration i t _) = Declaration i t newSp

instance Spanned Binding where
  getSpan (Binding _ _ _ s) = s
  setSpan newSp (Binding i ps e _) = Binding i ps e newSp

instance Spanned BindingOrDec where
  getSpan (BindingBOD b) = getSpan b
  getSpan (DeclarationBOD d) = getSpan d
  setSpan newSp (BindingBOD b) = BindingBOD (setSpan newSp b)
  setSpan newSp (DeclarationBOD d) = DeclarationBOD (setSpan newSp d)

instance Spanned RecordItem where
  getSpan (Union _ s) = s
  getSpan (Sum _ s) = s
  setSpan newSp (Union ts _) = Union ts newSp
  setSpan newSp (Sum ds _) = Sum ds newSp

instance Spanned Statement where
  getSpan (BindingS _ s) = s
  getSpan (DeclarationS _ s) = s
  getSpan (ImplS _ _ _ s) = s
  getSpan (TraitS _ _ _ s) = s
  getSpan (RecordS _ _ _ s) = s
  getSpan (ImportS _ _ s) = s
  getSpan (FromImportS _ _ s) = s
  setSpan newSp (BindingS b _) = BindingS b newSp
  setSpan newSp (DeclarationS d _) = DeclarationS d newSp
  setSpan newSp (ImplS i t bs _) = ImplS i t bs newSp
  setSpan newSp (TraitS i is bs _) = TraitS i is bs newSp
  setSpan newSp (RecordS i is ris _) = RecordS i is ris newSp
  setSpan newSp (ImportS n mi _) = ImportS n mi newSp
  setSpan newSp (FromImportS n mis _) = FromImportS n mis newSp

instance Spanned Pattern where
  getSpan (TupleP _ s) = s
  getSpan (BindP _ s) = s
  getSpan (VariantP _ _ s) = s
  getSpan (DropP s) = s
  getSpan (LiteralP _ s) = s
  getSpan (CustomP _ _ _ s) = s
  setSpan newSp (LiteralP a _) = LiteralP a newSp
  setSpan newSp (TupleP a _) = TupleP a newSp
  setSpan newSp (BindP a _) = BindP a newSp
  setSpan newSp (VariantP n is _) = VariantP n is newSp
  setSpan newSp (DropP _) = DropP newSp
  setSpan newSp (CustomP p o p' _) = CustomP p o p' newSp

instance Spanned Literal where
  getSpan (IntegerL _ s) = s
  getSpan (FloatL _ s) = s
  getSpan (StringL _ s) = s
  setSpan newSp (IntegerL i _) = IntegerL i newSp
  setSpan newSp (FloatL f _) = FloatL f newSp
  setSpan newSp (StringL s _) = StringL s newSp

instance Spanned Type where
  getSpan (Infer s) = s
  getSpan (Never s) = s
  getSpan (NamespaceType _ s) = s
  getSpan (BinOpType _ _ _ s) = s
  getSpan (TupleType _ s) = s
  setSpan newSp (Infer _) = Infer newSp
  setSpan newSp (Never _) = Never newSp
  setSpan newSp (NamespaceType n _) = NamespaceType n newSp
  setSpan newSp (BinOpType t o t' _) = BinOpType t o t' newSp
  setSpan newSp (TupleType ts _) = TupleType ts newSp

instance Spanned Expr where
  getSpan (LiteralE _ s) = s
  getSpan (BinOpE _ _ _ s) = s
  getSpan (TupleE _ s) = s
  getSpan (CondE _ _ _ s) = s
  getSpan (LetInE _ _ s) = s
  getSpan (VariableE _ s) = s
  getSpan (LambdaE _ _ s) = s
  getSpan (GroupedE _ s) = s

  setSpan newSp (LiteralE l _) = LiteralE l newSp
  setSpan newSp (BinOpE l o r _) = BinOpE l o r newSp
  setSpan newSp (TupleE es _) = TupleE es newSp
  setSpan newSp (CondE i ei e _) = CondE i ei e newSp
  setSpan newSp (LetInE e e' _) = LetInE e e' newSp
  setSpan newSp (VariableE n _) = VariableE n newSp
  setSpan newSp (LambdaE is e _) = LambdaE is e newSp
  setSpan newSp (GroupedE e _) = GroupedE e newSp
