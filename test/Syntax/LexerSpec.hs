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
import           Syntax.ParserGenericSpec
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
