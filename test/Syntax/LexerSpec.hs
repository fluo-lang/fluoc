module Syntax.LexerSpec where

import           Test.Hspec                     ( describe
                                                , it
                                                , shouldBe
                                                , Spec
                                                , Expectation
                                                )
import           Control.Applicative            ( Alternative(many) )
import           Syntax.Lexer
import           Syntax.ParserGeneric
import           Syntax.Token
import           Sources
import           Diagnostics
import           Display

testParser
  :: (Show a, Show b, Eq a, Eq b, Display b)
  => [b]
  -> Parser b a
  -> ParserReturn b a
  -> Expectation
testParser source (P fn) x = realRes `shouldBe` expectedRes
 where
  realRes     = fn source dummySpanLimited
  expectedRes = x

ns = Span (SourceId 0)

spec :: Spec
spec = do
  describe "Syntax.Lexer.ignored" $ do
    it "should ignore whitespace" $ testParser
      "\t    \t"
      (() <$ many ignored)
      (("", mapSpanLimited (+ 6) dummySpanLimited), Right ())
  describe "Syntax.Lexer.getTokens" $ do
    it "should ignore whitespace" $ getTokens "a   b" `shouldBe` Right
      [ Token (Ident "a") (ns 0 1)
      , Token (Ident "b") (ns 4 5)
      ]
  describe "Syntax.Lexer.ident" $ do
    it "should parse ident starting with `_`" $ testParser
      "_a123"
      ident
      ( ("", mapSpanLimited (+ 5) dummySpanLimited)
      , Right $ Token (Ident "_a123") (ns 0 5)
      )
    it "should parse ident ending with with `'`" $ testParser
      "a123''"
      ident
      ( ("", mapSpanLimited (+ 6) dummySpanLimited)
      , Right $ Token (Ident "a123''") (ns 0 6)
      )
    it "should parse ident ending with with `?`" $ testParser
      "a123??"
      ident
      ( ("", mapSpanLimited (+ 6) dummySpanLimited)
      , Right $ Token (Ident "a123??") (ns 0 6)
      )
    it "should parse ident starting with `a`" $ testParser
      "a123"
      ident
      ( ("", mapSpanLimited (+ 4) dummySpanLimited)
      , Right $ Token (Ident "a123") (ns 0 4)
      )
    it "should fail ident starting with `1`" $ testParser
      "1a23"
      ident
      ( ("1a23", dummySpanLimited)
      , Left $ Diagnostics [syntaxErr dummySpan "unexpected character `1`"]
      )
  describe "Syntax.Lexer.float" $ do
    it "should lex the float" $ testParser
      "123.123"
      float
      ( ("", mapSpanLimited (+ 7) dummySpanLimited)
      , Right (Token (Real "123.123") (ns 0 7))
      )
    it "should fail if dot is not there" $ testParser
      "123"
      float
      ( ("", mapSpanLimited (+ 3) dummySpanLimited)
      , Left $ Diagnostics [syntaxErr (ns 3 4) "unexpected end of file"]
      )
  describe "Syntax.Lexer.number" $ do
    it "should lex the number" $ testParser
      "123"
      number
      ( ("", mapSpanLimited (+ 3) dummySpanLimited)
      , Right (Token (Number "123") (ns 0 3))
      )
    it "should fail if not a number" $ testParser
      "abc"
      number
      ( ("abc", dummySpanLimited)
      , Left $ Diagnostics [syntaxErr dummySpan "unexpected character `a`"]
      )
  describe "Syntax.Lexer.singeLineComment" $ do
    it "should eat up the comment" $ testParser
      "# test asd128e9qdjad\n10"
      singeLineComment
      ( ("10", mapSpanLimited (+ 21) dummySpanLimited)
      , Right " test asd128e9qdjad"
      )
  describe "Syntax.Lexer.multiLineComment" $ do
    it "should eat up the comment" $ testParser
      "/#\nhello\nworld\nhuh123! \n\n#/10"
      multiLineComment
      ( ("10", mapSpanLimited (+ 27) dummySpanLimited)
      , Right "\nhello\nworld\nhuh123! \n\n"
      )
