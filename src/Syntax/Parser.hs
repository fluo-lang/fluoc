{-# LANGUAGE TupleSections #-}
module Syntax.Parser where

import           Diagnostics
import           Sources                        ( Span, mapSpan )
import           Syntax.Ast

newtype Parser a = P { unP :: String -> Span -> (String, Span, Either Diagnostics a) }

instance Functor Parser where
  fmap f (P st) = P $ \stream span -> case st stream span of
    (new, span, Left e   ) -> (new, span, Left e)
    (new, span, Right res) -> (new, span, Right (f res))

instance Applicative Parser where
  pure a = P $ \stream span -> (stream, span, Right a)
  P fst <*> P snd = P $ \stream span -> case fst stream span of
    (stream1, span1, Left e ) -> (stream1, span1, Left e)
    (stream1, span1, Right f) -> case snd stream1 span1 of
      (stream2, span2, Left e   ) -> (stream2, span2, Left e)
      (stream2, span2, Right res) -> (stream2, span2, Right (f res))

syntaxErr :: Span -> String -> Diagnostic
syntaxErr span msg =
  Diagnostic Error SyntaxError [Annotation span (Just msg) Error] span

satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = P $ \stream span -> case stream of
  [] ->
    (stream, span, Left $ Diagnostics [syntaxErr span "unexpected end of file"])
  x : xs -> if pred x
    then (xs, mapSpan (+ 1) span, Right x)
    else (stream, span, Left $ Diagnostics [syntaxErr span "unexpected character"])

-- try :: Parser a -> Parser a
-- orElse, (<|>) :: Parser a -> Parser a -> Parser a

parse :: Failable Block
parse = ()
