{-# LANGUAGE DeriveDataTypeable #-}
module Syntax.Token where

import           Sources                        ( Span )
import           Data.Data                      ( Data
                                                , toConstr
                                                )
import           Data.Function                  ( on )
import           Display

data Token = Token TokenRaw Span
  deriving Eq
instance Show Token where
  show (Token raw _) = show raw

instance Display Token where
  display t = show t

data TokenRaw = Ident String
           | Str String
           | Number String
           | Real String
           | Operator String

           | Let
           | Return
           | Import
           | Rec
           | Impl
           | Trait
           | Dec
           | In
           | If
           | Else
           | Match

           | DoubleQoute
           | SingleQoute

           | LBracket
           | RBracket
           | LParen
           | RParen
           | LCurly
           | RCurly
           deriving (Eq, Data)

instance Display TokenRaw where
  display t = show t

instance Show TokenRaw where
  show tok = case tok of
    Ident    val -> "identifier `" ++ val ++ "`"
    Str      val -> "string `" ++ val ++ "`"
    Number   val -> "number `" ++ val ++ "`"
    Operator val -> "operator `" ++ val ++ "`"

    _ ->
      "token `"
        ++ (case tok of
             Let         -> "let"
             Return      -> "return"
             Import      -> "import"
             Rec         -> "rec"
             Impl        -> "impl"
             Trait       -> "trait"
             Dec         -> "dec"
             In          -> "in"
             If          -> "if"
             Else        -> "else"
             Match       -> "match"

             DoubleQoute -> "\""
             SingleQoute -> "'"

             LBracket    -> "["
             RBracket    -> "]"
             LParen      -> "("
             RParen      -> ")"
             LCurly      -> "{"
             RCurly      -> "}"
           )
        ++ "`"

class VariantEq a where
  variantEq :: a -> a -> Bool

instance VariantEq TokenRaw where
  variantEq = (==) `on` toConstr

instance VariantEq Token where
  variantEq (Token a _) (Token b _) = a `variantEq` b
