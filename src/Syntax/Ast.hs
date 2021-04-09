{-# LANGUAGE FlexibleInstances, DeriveFunctor, StandaloneDeriving, DeriveDataTypeable #-}

module Syntax.Ast where

import           Sources

import           Data.Data                      ( Data
                                                , Typeable
                                                )
import           Data.List                      ( intercalate )

import           Data.Generics.Uniplate.Data    ( )
import           Display
import           Text.Printf                    ( printf )

addFixivity :: String -> String -> String
addFixivity = printf "`%s` (%s)"

data Ident = Ident String Span
           | OpId Fixivity Operator
           deriving (Show, Data, Typeable)
instance Display Ident where
  display (Ident i   _              ) = i
  display (OpId  fix (Operator op _)) = addFixivity op $ display fix
instance Eq Ident where
  (Ident a _) == (Ident b  _) = a == b
  (OpId  f a) == (OpId  f' b) = a == b && f == f'
  _           == _            = False
instance Ord Ident where
  (Ident a _) `compare` (Ident b _) = a `compare` b
  (OpId  f a) `compare` (OpId  g b) = (a, f) `compare` (b, g)
  OpId{}      `compare` Ident{}     = GT
  Ident{}     `compare` OpId{}      = LT

data Operator = Operator String Span
  deriving (Show, Data, Typeable)
instance Eq Operator where
  (Operator a _) == (Operator b _) = a == b
instance Ord Operator where
  (Operator a _) `compare` (Operator b _) = a `compare` b

data Namespace = Namespace [Ident] Span
  deriving (Show, Data, Typeable)
instance Eq Namespace where
  (Namespace a _) == (Namespace b _) = a == b
instance Ord Namespace where
  (Namespace a _) `compare` (Namespace b _) = a `compare` b
instance Display Namespace where
  display (Namespace a _) = intercalate "." $ display <$> a

data Declaration = Declaration Ident Type Span
  deriving (Eq, Show, Data, Typeable)
data Binding = Binding (Maybe Ident) [Pattern] Expr Span
  deriving (Eq, Show, Data, Typeable)
data RecordItem = Product Ident [Type] Span
                | NamedProduct Ident [Declaration] Span
                deriving (Eq, Show, Data, Typeable)

data Associativity = LeftA
                   | RightA
                   | Nonassoc
                   deriving (Eq, Show, Data, Typeable)

data Fixivity = BinaryF
              | PrefixF
              | PostfixF
              deriving (Eq, Ord, Data, Typeable, Show)

instance Display Fixivity where
  display BinaryF  = "binary"
  display PrefixF  = "prefix"
  display PostfixF = "postfix"

type Prec = Integer
data OpInfo = Prefix Prec | Postfix Prec | Binary Prec Associativity
  deriving (Eq, Show, Data, Typeable)

data Statement = BindingS [Binding] Span
               | DeclarationS Declaration Span
               | ImplS Namespace Type [Statement] Span
               | TraitS Ident [PolyIdent] [Statement] Span
               | RecordS Ident [PolyIdent] [RecordItem] Span
               | ImportS Namespace (Maybe Ident) Span
               | FromImportS Namespace (Maybe [Ident]) Span
               | OpDefS Operator OpInfo Span
               deriving (Eq, Show, Data, Typeable)

data Literal = IntegerL Integer Span
             | FloatL Double Span
             | StringL String Span
             deriving (Eq, Show, Data, Typeable)

data Type = Infer Span
          | Never Span
          | NamespaceType Namespace Span
          | TypeList (OpToks Type) Span
          | TupleType [Type] Span
          | OpType (Oped Type) Span
          | PolyType PolyIdent Span
          | TyApp Type Type Span
          deriving (Eq, Show, Data, Typeable)

data PolyIdent = PolyIdent String Span
  deriving (Eq, Show, Data, Typeable)

data OpTok a = OpTok Operator | OtherTok a
                deriving (Functor, Data, Typeable)
deriving instance Show a => Show (OpTok a)
deriving instance Eq a => Eq (OpTok a)

type Pattern = Expr

type OpToks a = [OpTok a]

data Expr = LiteralE Literal Span
          | ExprList (OpToks Expr) Span
          | OpE (Oped Expr) Span
          | TupleE [Expr] Span
          | CondE (Expr, Expr) [(Expr, Expr)] Expr Span
          | LetInE [Binding] Expr Span
          | VariableE Namespace Span
          | LambdaE [Pattern] Expr Span
          | MatchE Expr [MatchBranch] Span
          | FnAppE Expr Expr Span
          deriving (Show, Eq, Data, Typeable)

id2ns :: Ident -> Namespace
id2ns i = Namespace [i] s where s = getSpan i

data MatchBranch = MatchBranch Pattern Expr Span
  deriving (Eq, Show, Data, Typeable)

data Oped a = BinOp Operator a a
            | PreOp Operator a
            | PostOp Operator a
            deriving (Eq, Show, Functor, Data, Typeable)

instance Spanned Ident where
  getSpan (Ident _ s) = s
  getSpan (OpId  _ o) = getSpan o

instance Spanned Operator where
  getSpan (Operator _ s) = s

instance Spanned Namespace where
  getSpan (Namespace _ s) = s

instance Spanned Declaration where
  getSpan (Declaration _ _ s) = s

instance Spanned Binding where
  getSpan (Binding _ _ _ s) = s

instance Spanned RecordItem where
  getSpan (Product      _ _ s) = s
  getSpan (NamedProduct _ _ s) = s

instance Spanned a => Spanned (OpTok a) where
  getSpan (OpTok    o) = getSpan o
  getSpan (OtherTok e) = getSpan e

instance Spanned PolyIdent where
  getSpan (PolyIdent _ s) = s

instance Spanned Statement where
  getSpan (BindingS     _ s ) = s
  getSpan (DeclarationS _ s ) = s
  getSpan (ImplS   _ _ _ s  ) = s
  getSpan (TraitS  _ _ _ s  ) = s
  getSpan (RecordS _ _ _ s  ) = s
  getSpan (ImportS     _ _ s) = s
  getSpan (FromImportS _ _ s) = s
  getSpan (OpDefS      _ _ s) = s

instance Spanned Literal where
  getSpan (IntegerL _ s) = s
  getSpan (FloatL   _ s) = s
  getSpan (StringL  _ s) = s

instance Spanned Type where
  getSpan (Infer s          ) = s
  getSpan (Never s          ) = s
  getSpan (NamespaceType _ s) = s
  getSpan (OpType        _ s) = s
  getSpan (TypeList      _ s) = s
  getSpan (TupleType     _ s) = s
  getSpan (PolyType      _ s) = s
  getSpan (TyApp _ _ s      ) = s

instance Spanned Expr where
  getSpan (LiteralE _ s ) = s
  getSpan (OpE      _ s ) = s
  getSpan (TupleE   _ s ) = s
  getSpan (CondE _ _ _ s) = s
  getSpan (LetInE _ _ s ) = s
  getSpan (VariableE _ s) = s
  getSpan (LambdaE _ _ s) = s
  getSpan (MatchE  _ _ s) = s
  getSpan (ExprList _ s ) = s
  getSpan (FnAppE _ _ s ) = s
