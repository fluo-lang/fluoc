{-# LANGUAGE RankNTypes, InstanceSigs #-}
module Syntax.ParserGeneric where

import           Control.Applicative            ( Alternative(..)
                                                , optional
                                                )

import           Diagnostics                    ( Diagnostics(..)
                                                , Diagnostic(Diagnostic)
                                                , Annotation(Annotation)
                                                , DiagnosticKind(SyntaxError)
                                                , DiagnosticType(Error)
                                                )
import           Sources                        ( SourceId(..)
                                                , Span(..)
                                                , dummySpan
                                                )
import           Syntax.Token                   ( Token(..)
                                                , VariantEq
                                                , variantEq
                                                )
import           Display

data SpanLimited = SpanLimited SourceId Int
  deriving (Show, Eq)

toSpan :: SpanLimited -> Int -> Span
toSpan (SpanLimited id s) len = Span id s (s + len)

toSpanSE :: SpanLimited -> SpanLimited -> Span
toSpanSE (SpanLimited _ s) (SpanLimited id e) = Span id s e

mapSpanLimited :: (Int -> Int) -> SpanLimited -> SpanLimited
mapSpanLimited f (SpanLimited id s) = SpanLimited id (f s)

dummySpanLimited :: SpanLimited
dummySpanLimited = SpanLimited (SourceId 0) 0

-- The remaining string and span, not sure if "parser context" is the right word
type ParserContext s = ([s], SpanLimited)

-- Return result of the parser
type ParserReturn s a = (ParserContext s, Either Diagnostics a)

type ParserFn s a = [s] -> SpanLimited -> ParserReturn s a

type StringParser a = Parser Char a
type TokenParser a = Parser Token a

-- A parser applicative and functor, "wrapper" around source stream, span, and result
newtype Parser s a = P { unP :: (Display s) => ParserFn s a}

-- Map value behind the parser
instance Functor (Parser s) where
  fmap f (P st) = P $ \stream span -> case st stream span of
    (state, Left e   ) -> (state, Left e)
    (state, Right res) -> (state, Right (f res))

-- Apply a function in the parser to another parser
instance Applicative (Parser s) where
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

class SatisfyParser a where
  changeSpan :: SpanLimited -> a -> SpanLimited

instance SatisfyParser Char where
  changeSpan span _ = mapSpanLimited (+ 1) span

instance SatisfyParser Token where
  changeSpan _ (Token _ (Span id _ e)) = SpanLimited id e

-- Satisfy a certian function, otherwise error
satisfy :: (SatisfyParser s) => (s -> Bool) -> Parser s s
satisfy pred = P $ \stream span -> case stream of
  [] ->
    ( (stream, span)
    , Left $ Diagnostics [syntaxErr (toSpan span 1) "unexpected end of file (EOF)"]
    )
  x : xs -> if pred x
    then ((xs, changeSpan span x), Right x)
    else
      ( (stream, span)
      , Left
        $ Diagnostics [syntaxErr (toSpan span 1) ("unexpected " ++ display x)]
      )

-- Try apply a parser, but backtrack if failed
try :: Parser s a -> Parser s a
try (P a) = P $ \stream span -> case a stream span of
  (_    , Left e   ) -> ((stream, span), Left e)
  (state, Right res) -> (state, Right res)

-- Try to use `P a`, but if it fails return the result of `P b`
orElse :: Parser s a -> Parser s a -> Parser s a
orElse (P a) (P b) = P $ \stream span -> case a stream span of
  ((stream', span'), Left e   ) -> b stream' span'
  (state           , Right res) -> (state, Right res)

-- Parse 0 or more
manyParser :: (Display s) => Parser s a -> Parser s [a]
manyParser (P a) = P go where
  go stream span = case a stream span of
    (_               , Left e   ) -> ((stream, span), Right [])
    ((stream', span'), Right res) -> case go stream' span' of
      (_    , Left e    ) -> ((stream, span), Left e)
      (state, Right res') -> (state, Right (res : res'))

-- Parse 1 or more
someParser :: (Display s) => Parser s a -> Parser s [a]
someParser (P a) = P go where
  go stream span = case a stream span of
    (_, Left e) -> ((stream, span), Left e)
    ((stream', span'), Right res) ->
      let (P manyP) = manyParser (P a)
      in  case manyP stream' span' of
            ((stream'', span''), Left e    ) -> ((stream, span), Left e)
            (state             , Right res') -> (state, Right (res : res'))

-- Destructure a parser
dstrParser :: (Display s) => Parser s a -> ParserFn s a
dstrParser (P a) = a

instance Monad (Parser s) where
  (>>=) (P a) fn = P $ \stream span -> case a stream span of
    (state           , Left e   ) -> (state, Left e)
    ((stream', span'), Right res) -> (dstrParser $ fn res) stream' span'
  (>>) (P a) (P b) = P $ \stream span -> case a stream span of
    (state           , Left e   ) -> (state, Left e)
    ((stream', span'), Right res) -> b stream' span'
  return = pure

instance (Display s) => Alternative (Parser s) where
  empty = P $ \stream span ->
    ( (stream, span)
    , Left $ Diagnostics
      [syntaxErr (toSpan span 1) "internal error: empty alternative"]
    )
  (<|>) = orElse

  many  = manyParser
  some  = someParser

combine, (<||>) :: Parser s a -> Parser s b -> Parser s b
combine (P a) (P b) = P $ \stream span -> case a stream span of
  (state           , Left e   ) -> (state, Left e)
  ((stream', span'), Right res) -> case b stream' span' of
    (state, Left e    ) -> (state, Left e)
    (state, Right res') -> (state, Right res')

(<||>) = combine

getParserState :: Parser s a -> Parser s (ParserContext s)
getParserState (P fn) =
  P $ \stream span -> let a = fst (fn stream span) in (a, Right a)

maybeAnd :: Parser s (Maybe a) -> Parser s [a] -> Parser s [a]
maybeAnd (P a) (P b) = P $ \stream span -> case a stream span of
  (state           , Left e        ) -> (state, Left e)
  (state           , Right Nothing ) -> (state, Right [])
  ((stream', span'), Right (Just x)) -> case b stream' span' of
    (state, Left e  ) -> (state, Left e)
    (state, Right xs) -> (state, Right (x : xs))

getSpan :: Parser s SpanLimited
getSpan = snd <$> getParserState (pure ())

getSpan' :: Parser s a -> Parser s SpanLimited
getSpan' a = snd <$> getParserState a

withSpan :: Parser s (Span -> a) -> Parser s a
withSpan p = do
  span <- getSpan
  val  <- p
  val . toSpanSE span <$> getSpan

match :: (SatisfyParser s, Eq s) => s -> Parser s s
match c = satisfy (== c)

matchVariant :: (SatisfyParser s, VariantEq s) => s -> Parser s s
matchVariant c = satisfy (variantEq c)

oneOf :: (SatisfyParser s, Eq s) => [s] -> Parser s s
oneOf cs = satisfy (`elem` cs)

string :: (SatisfyParser s, Eq s) => [s] -> Parser s [s]
string = foldr (\c -> (<*>) ((:) <$> match c)) (pure [])
