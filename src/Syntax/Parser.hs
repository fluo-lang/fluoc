{-# LANGUAGE TupleSections #-}
module Syntax.Parser where

import Control.Applicative ( Alternative(..), optional )
import Data.Char ( isDigit, isAsciiLower, isAsciiUpper )
import           Data.Maybe                     ( fromMaybe )
import           Data.Tuple                     ( swap )

import Diagnostics
    ( Diagnostics(..),
      Diagnostic(Diagnostic),
      Annotation(Annotation),
      DiagnosticKind(SyntaxError),
      DiagnosticType(Error) )
import Sources ( SourceId(..), Span(..), dummySpan )
import Syntax.Ast
    ( Expr(Number),
      Type(Infer, NamespaceType),
      Arguments(..),
      Statement(FunDecl),
      Block(Block),
      Namespace(..),
      Ident(..) )

data SpanLimited = SpanLimited Int SourceId
  deriving (Show, Eq)

toSpan :: SpanLimited -> Int -> Span
toSpan (SpanLimited s id) len = Span s (s + len) id

toSpanSE :: SpanLimited -> SpanLimited -> Span
toSpanSE (SpanLimited s _) (SpanLimited e id) = Span s e id

mapSpanLimited :: (Int -> Int) -> SpanLimited -> SpanLimited
mapSpanLimited f (SpanLimited s id) = SpanLimited (f s) id

dummySpanLimited :: SpanLimited
dummySpanLimited = SpanLimited 0 (SourceId 0)

-- The remaining string and span, not sure if "parser context" is the right word
type ParserContext = (String, SpanLimited)

-- Return result of the parser
type ParserReturn a = (ParserContext, Either Diagnostics a)

type ParserFn a = String -> SpanLimited -> ParserReturn a

-- A parser applicative and functor, "wrapper" around source stream, span, and result
newtype Parser a = P { unP :: ParserFn a}

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
    , Left $ Diagnostics [syntaxErr (toSpan span 1) "unexpected end of file"]
    )
  x : xs -> if pred x
    then ((xs, mapSpanLimited (+ 1) span), Right x)
    else
      ( (stream, span)
      , Left $ Diagnostics
        [syntaxErr (toSpan span 1) ("unexpected character `" ++ [x] ++ "`")]
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

-- Destructure a parser
dstrParser :: Parser a -> ParserFn a
dstrParser (P a) = a

instance Monad Parser where
  (>>=) (P a) fn = P $ \stream span -> case a stream span of
    (state           , Left e   ) -> (state, Left e)
    ((stream', span'), Right res) -> (dstrParser $ fn res) stream' span'
  (>>) (P a) (P b) = P $ \stream span -> case a stream span of
    (state           , Left e   ) -> (state, Left e)
    ((stream', span'), Right res) -> b stream' span'
  return = pure

instance Alternative Parser where
  empty = P $ \stream span ->
    ( (stream, span)
    , Left $ Diagnostics
      [syntaxErr (toSpan span 1) "internal error: empty alternative"]
    )
  (<|>) = orElse

  many  = manyParser
  some  = someParser

combine, (<||>) :: Parser a -> Parser b -> Parser b
combine (P a) (P b) = P $ \stream span -> case a stream span of
  (state           , Left e   ) -> (state, Left e)
  ((stream', span'), Right res) -> case b stream' span' of
    (state, Left e    ) -> (state, Left e)
    (state, Right res') -> (state, Right res')

(<||>) = combine

getParserState :: Parser a -> Parser ParserContext
getParserState (P fn) =
  P $ \stream span -> let a = fst (fn stream span) in (a, Right a)

maybeAnd :: Parser (Maybe a) -> Parser [a] -> Parser [a]
maybeAnd (P a) (P b) = P $ \stream span -> case a stream span of
  (state           , Left e        ) -> (state, Left e)
  (state           , Right Nothing ) -> (state, Right [])
  ((stream', span'), Right (Just x)) -> case b stream' span' of
    (state, Left e  ) -> (state, Left e)
    (state, Right xs) -> (state, Right (x : xs))

getSpan :: Parser SpanLimited
getSpan = snd <$> getParserState (pure ())

getSpan' :: Parser a -> Parser SpanLimited
getSpan' a = snd <$> getParserState a

withSpan :: Parser (Span -> a) -> Parser a
withSpan p = do
  span <- getSpan
  val  <- p
  val . toSpanSE span <$> getSpan

char :: Char -> Parser Char
char c = satisfy (== c)

oneOf :: [Char] -> Parser Char
oneOf cs = satisfy (`elem` cs)

string :: String -> Parser String
string = foldr (\c -> (<*>) ((:) <$> char c)) (pure [])

asciiAlpha :: Parser Char
asciiAlpha = satisfy (\a -> isAsciiUpper a || isAsciiLower a)

asciiAlphaUnderscore :: Parser Char
asciiAlphaUnderscore = asciiAlpha <|> char '_'

asciiAlphaNumeric :: Parser Char
asciiAlphaNumeric = asciiAlpha <|> satisfy isDigit

number = someParser (satisfy isDigit)

ident :: Parser Ident
ident = withSpan $ do
  span   <- getSpan
  ident' <- (:) <$> asciiAlphaUnderscore <*> many asciiAlphaNumeric
  span'  <- getSpan
  return $ Ident ident'

namespace :: Parser Namespace
namespace = withSpan $ do
  ident' <- ident
  others <- many (dot <||> ident)
  return $ Namespace (ident' : others)

letTok = string "let"
returnTok = string "return"
importTok = string "import"
recTok = string "rec"
funTok = string "fun"
instTok = string "inst"
classTok = string "class"

colon = char ':'
comma = char ','
equals = char '='
dot = char '.'
arrow = string "->"
pipe = char '|'
underscore = char '_'
eqcolon = string "=:"
dotdotdot = string "..."

lbracket = char '['
rbracket = char ']'
lparen = char '('
rparen = char ')'
lcurly = char '{'
rcurly = char '}'

infer :: Parser Type
infer = Infer <$ underscore

namespaceTy :: Parser Type
namespaceTy = NamespaceType <$> namespace

ty :: Parser Type
ty = infer <|> namespaceTy

positionalArgument :: Parser (Ident, Type)
positionalArgument =
  (,) <$> ident <*> (fromMaybe Infer <$> optional (colon <||> ty))

positionalArguments :: Parser [(Ident, Type)]
positionalArguments =
  maybeAnd (optional positionalArgument) (many (comma <||> positionalArgument))

mergeTuples :: a -> (b, c) -> (a, b, c)
mergeTuples a (b, c) = (a, b, c)

expr :: Parser Expr
expr = pure $ Number "10" dummySpan

keywordArguments :: Parser [(Ident, Type, Maybe Expr)]
keywordArguments =
  many
    $   mergeTuples
    <$> ident
    <*> (   ((, Nothing) <$> (eqcolon <||> ty))
        <|> ((Infer, ) <$> (Just <$> equals <||> expr))
        <|> (   swap
            <$> ((,) <$> (Just <$> (equals <||> expr)) <*> (colon <||> ty))
            )
        )

varargs :: Parser (Ident, Type)
varargs = dotdotdot <||> ((,) <$> ident <*> (colon <||> ty))

arguments :: Parser Arguments
arguments =
  withSpan $ Arguments <$> positionalArguments <*> keywordArguments <*> varargs

parseFunc :: Parser Statement
parseFunc = do
  name <- ident
  lparen
  args <- arguments
  rparen
  type' <- ty
  return $ FunDecl name args type' (Block []) dummySpan

parse :: ()
parse = ()
