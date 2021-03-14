module Syntax.LexerSpec where

import           Test.Hspec                     ( describe
                                                , it
                                                , shouldBe
                                                , Spec
                                                , Expectation
                                                )
import           Syntax.Lexer
import           Syntax.ParserGeneric
import           Syntax.Token
import           Sources
import           Diagnostics
import           TestUtil

spec :: Spec
spec = do
  describe "Syntax.Lexer.ident" $ do
    it "should parse ident starting with `_`" $ testParser
      "_a123"
      ident
      ( ("", mapSpanLimited (+ 5) dummySpanLimited)
      , Right $ Token (Ident "_a123") (Span (SourceId 0) 0 5)
      )
    it "should parse ident starting with `a`" $ testParser
      "a123"
      ident
      ( ("", mapSpanLimited (+ 4) dummySpanLimited)
      , Right $ Token (Ident "a123") (Span (SourceId 0) 0 4)
      )
    it "should fail ident starting with `1`" $ testParser
      "1a23"
      ident
      ( ("1a23", dummySpanLimited)
      , Left $ Diagnostics [syntaxErr dummySpan "unexpected character `1`"]
      )
  describe "Syntax.Lexer.number" $ do
    it "should lex the number" $ testParser
      "123"
      number
      ( ("", mapSpanLimited (+ 3) dummySpanLimited)
      , Right (Token (Number "123") (Span (SourceId 0) 0 3))
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
