module Syntax.Lexer where

import           Control.Applicative            ( Alternative(..) )
import           Syntax.ParserGeneric
import           Syntax.Token
import           Sources                        ( Span )

import           Data.Char                      ( isDigit
                                                , isAsciiLower
                                                , isAsciiUpper
                                                )
asciiAlpha :: StringParser Char
asciiAlpha = satisfy (\a -> isAsciiUpper a || isAsciiLower a)

asciiAlphaUnderscore :: StringParser Char
asciiAlphaUnderscore = asciiAlpha <|> match '_'

asciiAlphaNumeric :: StringParser Char
asciiAlphaNumeric = asciiAlpha <|> satisfy isDigit

number = someParser (satisfy isDigit)

ident :: StringParser Token
ident = withSpan $ do
  span   <- getSpan
  ident' <- (:) <$> asciiAlphaUnderscore <*> many asciiAlphaNumeric
  span'  <- getSpan
  return $ Token $ Ident ident'

spannedConst :: StringParser a -> (Span -> Token) -> StringParser Token
spannedConst s t = withSpan $ do
  s
  return t

letTok = spannedConst (string "let") (Token Let)
decTok = spannedConst (string "dec") (Token Dec)
returnTok = spannedConst (string "return") (Token Return)
importTok = spannedConst (string "import") (Token Import)
recTok = spannedConst (string "rec") (Token Return)
traitTok = spannedConst (string "trait") (Token Trait)
implTok = spannedConst (string "impl") (Token Impl)

colon = spannedConst (match ':') (Token Colon)
comma = spannedConst (match ',') (Token Comma)
equals = spannedConst (match '=') (Token Equals)
dot = spannedConst (match '.') (Token Dot)
arrow = spannedConst (string "->") (Token Arrow)
pipe = spannedConst (match '|') (Token Pipe)
underscore = spannedConst (match '_') (Token Underscore)
eqcolon = spannedConst (string "=:") (Token EqColon)
dotdotdot = spannedConst (string "...") (Token DotDotDot)

lbracket = spannedConst (match '[') (Token LBracket)
rbracket = spannedConst (match ']') (Token RBracket)
lparen = spannedConst (match '(') (Token LParen)
rparen = spannedConst (match ')') (Token RParen)
lcurly = spannedConst (match '{') (Token LCurly)
rcurly = spannedConst (match '}') (Token RCurly)
