{-# LANGUAGE TupleSections, LambdaCase #-}
module Syntax.Parser where

import           Control.Applicative            ( Alternative(..)
                                                , optional
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Data.Tuple                     ( swap )

import           Syntax.ParserGeneric
import           Syntax.Ast                     ( Expr(Number)
                                                , Type(..)
                                                , Arguments(..)
                                                , Statement(FunDecl)
                                                , Block(Block)
                                                , Namespace(..)
                                                , Ident(..)
                                                )
import           Sources                        ( SourceId(..)
                                                , Span(..)
                                                , dummySpan
                                                )
import qualified Syntax.Token                  as T

ident :: TokenParser Ident
ident = do
  tok <- matchVariant (T.Token (T.Ident undefined) undefined)
  return $ case tok of
    T.Token (T.Ident val) span -> Ident val span

token :: T.TokenRaw -> TokenParser ()
token tok = () <$ matchVariant (T.Token tok undefined)

matchIdent :: String -> TokenParser ()
matchIdent s = () <$ satisfy
  (\case
    (T.Token (T.Ident s') _) -> s' == s
    _                         -> False
  )

matchOperator :: String -> TokenParser ()
matchOperator s = () <$ satisfy
  (\case
    (T.Token (T.Operator s') _) -> s' == s
    _                         -> False
  )

colonColon = matchOperator "::"
underscore = matchIdent "_"

namespace :: TokenParser Namespace
namespace = withSpan $ do
  ident' <- ident
  others <- many (colonColon <||> ident)
  return $ Namespace (ident' : others)

namespaceTy :: TokenParser (Span -> Type)
namespaceTy = NamespaceType <$> namespace

typeApplication :: TokenParser (Span -> Type)
typeApplication = do
  namespace <- namespace 
  tys <- some ty
  return $ TypeApplication namespace tys

infer :: TokenParser (Span -> Type)
infer = Infer <$ underscore 

ty :: TokenParser Type
ty = withSpan $ infer <|> try typeApplication <|> namespaceTy

parse :: ()
parse = ()
