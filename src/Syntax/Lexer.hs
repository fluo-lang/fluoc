{-# LANGUAGE FlexibleContexts, MonoLocalBinds #-}

module Syntax.Lexer where

import           Control.Applicative            ( Alternative(..) )
import           Syntax.ParserGeneric
import           Syntax.Token
import           Sources                        ( Span )

import           Data.Char                      ( isDigit
                                                , isAsciiLower
                                                , isAsciiUpper
                                                )
import           Diagnostics                    ( Diagnostics )

asciiAlpha :: StringParser Char
asciiAlpha = satisfy (\a -> isAsciiUpper a || isAsciiLower a)

idStart :: StringParser Char
idStart = asciiAlpha <|> match '_'

idContinue :: StringParser Char
idContinue = asciiAlpha <|> satisfy isDigit <|> oneOf "'?"

number :: StringParser Token
number = withSpan $ Token . Number <$> someParser (satisfy isDigit)

float :: StringParser Token
float =
  withSpan
    $   Token
    .   Real
    <$> (do
          first <- someParser (satisfy isDigit)
          dot   <- match '.'
          last  <- someParser (satisfy isDigit)
          return $ first ++ (dot : last)
        )

operator :: StringParser Token
operator =
  withSpan $ Token . Operator <$> someParser (oneOf "+*-/<>|:$^@!~%&.,:=")

ident :: StringParser Token
ident = withSpan $ Token . Ident <$> ((:) <$> idStart <*> many idContinue)

spannedConst :: StringParser a -> (Span -> Token) -> StringParser Token
spannedConst s t = withSpan $ do
  try s
  return t

letTok = spannedConst (string "let") (Token Let)
decTok = spannedConst (string "dec") (Token Dec)
returnTok = spannedConst (string "return") (Token Return)
importTok = spannedConst (string "import") (Token Import)
recTok = spannedConst (string "rec") (Token Return)
traitTok = spannedConst (string "trait") (Token Trait)
implTok = spannedConst (string "impl") (Token Impl)
ifTok = spannedConst (string "if") (Token If)
elseTok = spannedConst (string "else") (Token Else)
matchTok = spannedConst (string "match") (Token Match)

doubleQoute = spannedConst (match '"') (Token DoubleQoute)
singleQoute = spannedConst (match '\'') (Token SingleQoute)

lbracket = spannedConst (match '[') (Token LBracket)
rbracket = spannedConst (match ']') (Token RBracket)
lparen = spannedConst (match '(') (Token LParen)
rparen = spannedConst (match ')') (Token RParen)
lcurly = spannedConst (match '{') (Token LCurly)
rcurly = spannedConst (match '}') (Token RCurly)

anyToken :: StringParser Token
anyToken =
  letTok
    <|> decTok
    <|> returnTok
    <|> importTok
    <|> recTok
    <|> traitTok
    <|> implTok
    <|> ifTok
    <|> elseTok
    <|> matchTok
    <|> doubleQoute
    <|> singleQoute
    <|> lbracket
    <|> rbracket
    <|> lparen
    <|> rparen
    <|> lcurly
    <|> rcurly
    <|> try float
    <|> number
    <|> operator
    <|> ident

singeLineComment :: StringParser String
singeLineComment = do
  match '#'
  str <- many $ satisfy (/= '\n')
  match '\n'
  return str

multiLineComment :: StringParser String
multiLineComment =
  string "/#" <||> manyUntil (satisfy (const True)) (string "#/")

ignored :: StringParser ()
ignored =
  (() <$ oneOf " \t") <|> (() <$ (singeLineComment <|> multiLineComment))

multiple :: StringParser [Token]
multiple = many (many ignored <||> anyToken)

getTokens :: String -> Either Diagnostics [Token]
getTokens s = snd $ lex s dummySpanLimited where P lex = multiple
