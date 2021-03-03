module Syntax.ParserSpec where

import           Control.Applicative
import           Test.Hspec
import           Test.QuickCheck
import           Control.Exception              ( evaluate )
import           Syntax.Parser
import           Diagnostics
import           Sources                        ( dummySpan
                                                , mapSpan
                                                )

testParser
  :: (Show a, Eq a)
  => String
  -> Parser a
  -> (ParserContext, Either Diagnostics a)
  -> Expectation
testParser source (P fn) x = fn source dummySpan `shouldBe` x

spec :: Spec
spec = do
  describe "Syntax.Parser.Parser" $ do
    it "should map properly" $ testParser
      "a"
      (fmap (\a -> a : ['b']) (char 'a'))
      (("", mapSpan (+ 1) dummySpan), Right "ab")
    it "should apply properly" $ testParser
      "a"
      (pure (\a -> a : ['b']) <*> (char 'a'))
      (("", mapSpan (+ 1) dummySpan), Right "ab")

  describe "Syntax.Parser.satisfy" $ do
    it "should give character on satisfy" $ testParser
      "n"
      (satisfy (== 'n'))
      (("", mapSpan (+ 1) dummySpan), Right 'n')
    it "should give error on satisfy if there's an eof" $ testParser
      ""
      (satisfy (== 'n'))
      ( ("", dummySpan)
      , Left $ Diagnostics [syntaxErr dummySpan "unexpected end of file"]
      )
    it "should give error if not satisfied" $ testParser
      "e"
      (satisfy (== 'n'))
      ( ("e", dummySpan)
      , Left $ Diagnostics [syntaxErr dummySpan "unexpected character `e`"]
      )
  describe "Syntax.Parser.try" $ do
    it "should backtrack on try" $ testParser
      "ne"
      (try (string "nw"))
      ( ("ne", dummySpan)
      , Left $ Diagnostics
        [syntaxErr (mapSpan (+ 1) dummySpan) "unexpected character `e`"]
      )

    it "should work on try backtrack with alternate" $ testParser
      "ne"
      (try (string "nw") <|> try (string "ne"))
      (("", mapSpan (+ 2) dummySpan), Right "ne")

  describe "Syntax.Parser.string" $ do
    it "should fail on alternate without try backtrack" $ testParser
      "ne"
      (string "nw" <|> string "ne")
      ( ("e", mapSpan (+ 1) dummySpan)
      , Left $ Diagnostics
        [syntaxErr (mapSpan (+ 1) dummySpan) "unexpected character `e`"]
      )

  describe "Syntax.Parser.manyParser" $ do
    it "should parse no characters" $ testParser
      "000"
      (manyParser (char 'a'))
      (("000", dummySpan), Right [])
    it "should parse multiple" $ testParser
      "000"
      (manyParser (char '0'))
      (("", mapSpan (+ 3) dummySpan), Right "000")

    it "should not fail multiple after 1" $ testParser
      "00b"
      (manyParser (char '0'))
      (("b", mapSpan (+ 2) dummySpan), Right "00")

  describe "Syntax.Parser.someParser" $ do
    it "should fail if there are no matches" $ testParser
      "b"
      (someParser (char '0'))
      ( ("b", dummySpan)
      , Left $ Diagnostics [syntaxErr dummySpan "unexpected character `b`"]
      )
    it "should parse if there is 1 match" $ testParser
      "0b"
      (someParser (char '0'))
      (("b", mapSpan (+ 1) dummySpan), Right "0")
    it "should parse if there is more than 1 match" $ testParser
      "00000b"
      (someParser (char '0'))
      (("b", mapSpan (+ 5) dummySpan), Right "00000")

  describe "Syntax.Parser.oneOf" $ do
    it "should choose between options with multiple options" $ testParser
      "acba"
      (manyParser (oneOf ['a', 'b', 'c']))
      (("", mapSpan (+ 4) dummySpan), Right "acba")
    it "should choose between options with one option" $ testParser
      "a"
      (oneOf ['a'])
      (("", mapSpan (+ 1) dummySpan), Right 'a')

  describe "Syntax.Parser.string" $ do
    it "should match a string exactly" $ testParser
      "test_keyword"
      (string "test_keyword")
      (("", mapSpan (+ 12) dummySpan), Right "test_keyword")
