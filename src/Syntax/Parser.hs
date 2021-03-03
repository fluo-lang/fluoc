{-# LANGUAGE TupleSections #-}
module Syntax.Parser where

import           Control.Applicative

import           Diagnostics
import           Sources                        ( Span
                                                , mapSpan
                                                )
import           Syntax.Ast

-- The remaining string and span, not sure if "parser context" is the right word
type ParserContext = (String, Span)

-- A parser applicative and functor, "wrapper" around source stream, span, and result
newtype Parser a = P { unP :: String -> Span -> (ParserContext, Either Diagnostics a) }

-- Map value behind the parser
instance Functor Parser where
  fmap f (P st) = P $ \stream span -> case st stream span of
    (state, Left e   ) -> (state, Left e)
    (state, Right res) -> (state, Right (f res))

-- Apply a function in the parser to another parser
instance Applicative Parser where
  pure a = P $ \stream span -> ((stream, span), Right a)
  P fst <*> P snd = P $ \stream span -> case fst stream span of
    (state           , Left e ) -> (state, Left e)
    ((stream', span'), Right f) -> case snd stream' span' of
      (state, Left e   ) -> (state, Left e)
      (state, Right res) -> (state, Right (f res))

-- Generate a syntax error with a given span and message
syntaxErr :: Span -> String -> Diagnostic
syntaxErr span msg =
  Diagnostic Error SyntaxError [Annotation span (Just msg) Error] span

-- Satisfy a certian function, otherwise error
satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = P $ \stream span -> case stream of
  [] ->
    ( (stream, span)
    , Left $ Diagnostics [syntaxErr span "unexpected end of file"]
    )
  x : xs -> if pred x
    then ((xs, mapSpan (+ 1) span), Right x)
    else
      ( (stream, span)
      , Left
        $ Diagnostics [syntaxErr span ("unexpected character `" ++ [x] ++ "`")]
      )

-- Try apply a parser, but backtrack if failed
try :: Parser a -> Parser a
try (P a) = P $ \stream span -> case a stream span of
  (_    , Left e   ) -> ((stream, span), Left e)
  (state, Right res) -> (state, Right res)

-- Try to use `P a`, but if it fails return the result of `P b`
orElse :: Parser a -> Parser a -> Parser a
orElse (P a) (P b) = P $ \stream span -> case a stream span of
  ((stream', span'), Left e   ) -> b stream' span'
  (state           , Right res) -> (state, Right res)

-- Parse 0 or more
manyParser :: Parser a -> Parser [a]
manyParser (P a) = P go where
  go stream span = case a stream span of
    (_               , Left e   ) -> ((stream, span), Right [])
    ((stream', span'), Right res) -> case go stream' span' of
      (_    , Left e    ) -> ((stream, span), Left e)
      (state, Right res') -> (state, Right (res : res'))

-- Parse 1 or more
someParser :: Parser a -> Parser [a]
someParser (P a) = P go where
  go stream span = case a stream span of
    (_, Left e) -> ((stream, span), Left e)
    ((stream', span'), Right res) ->
      let (P manyP) = manyParser (P a)
      in  case manyP stream' span' of
            ((stream'', span''), Left e    ) -> ((stream, span), Left e)
            (state             , Right res') -> (state, Right (res : res'))

instance Alternative Parser where
  empty = P $ \stream span ->
    ( (stream, span)
    , Left $ Diagnostics [syntaxErr span "internal error: empty alternative"]
    )
  (<|>) = orElse

  many  = manyParser
  some  = someParser

char :: Char -> Parser Char
char c = satisfy (== c)

oneOf :: [Char] -> Parser Char
oneOf cs = satisfy (`elem` cs)

string :: String -> Parser String
string = foldr (\c -> (<*>) ((:) <$> char c)) (pure [])

parse :: ()
parse = ()
