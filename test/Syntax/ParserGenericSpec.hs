module Syntax.ParserGenericSpec where

import           Control.Applicative            ( Alternative((<|>), many, some)
                                                , optional
                                                )
import           Data.Char                      ( isDigit )
import           Test.Hspec                     ( describe
                                                , it
                                                , shouldBe
                                                , Spec
                                                , Expectation
                                                )
import           Test.QuickCheck                ( )
import           Control.Exception              ( evaluate )
import           Syntax.Lexer
import           Syntax.ParserGeneric
import           Syntax.Token
import           Diagnostics                    ( Diagnostics(..) )
import           Sources                        ( Span(Span)
                                                , SourceId(SourceId)
                                                , dummySpan
                                                , mapSpan
                                                )

import           Display

{-# ANN spec "HLint: ignore Use <$>" #-}

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
spec :: Spec
spec = do
  describe "Syntax.ParserGeneric.Parser" $ do
    it "should map properly" $ testParser
      "a"
      (fmap (\a -> a : ['b']) (match 'a'))
      (("", mapSpanLimited (+ 1) dummySpanLimited), Right "ab")
    it "should apply properly" $ testParser
      "a"
      (pure (\a -> a : ['b']) <*> match 'a')
      (("", mapSpanLimited (+ 1) dummySpanLimited), Right "ab")
    it "should combine properly" $ testParser
      "aba123"
      (many (oneOf "ab") <||> many (satisfy isDigit))
      (("", mapSpanLimited (+ 6) dummySpanLimited), Right "123")
    it "should work monadically"
      $ let combined = do
              char <- many (oneOf ['a', 'b', 'c'])
              num  <- many (satisfy isDigit)
              return [char, num]
        in  testParser
              "cabc10"
              combined
              ( ("", mapSpanLimited (+ 6) dummySpanLimited)
              , Right ["cabc", "10"]
              )
    it "should all fail monadically if first bind fails"
      $ let combined = do
              char <- some (oneOf ['a', 'b', 'c'])
              num  <- many (satisfy isDigit)
              return [char, num]
        in  testParser
              "10cabc10"
              combined
              ( ("10cabc10", dummySpanLimited)
              , Left
                $ Diagnostics [syntaxErr dummySpan "unexpected character `1`"]
              )

  describe "Syntax.ParserGeneric.getSpan" $ do
    it "should return the span" $ testParser
      "abc1"
      (getSpan' ident)
      ( ("", mapSpanLimited (+ 4) dummySpanLimited)
      , Right $ SpanLimited (SourceId 0) 4
      )

  describe "Syntax.ParserGeneric.satisfy" $ do
    it "should give character on satisfy" $ testParser
      "n"
      (satisfy (== 'n'))
      (("", mapSpanLimited (+ 1) dummySpanLimited), Right 'n')
    it "should give error on satisfy if there's an eof" $ testParser
      ""
      (satisfy (== 'n'))
      ( ("", dummySpanLimited)
      , Left $ Diagnostics [syntaxErr dummySpan "unexpected end of file"]
      )
    it "should give error if not satisfied" $ testParser
      "e"
      (satisfy (== 'n'))
      ( ("e", dummySpanLimited)
      , Left $ Diagnostics [syntaxErr dummySpan "unexpected character `e`"]
      )
  describe "Syntax.ParserGeneric.try" $ do
    it "should backtrack on try" $ testParser
      "ne"
      (try (string "nw"))
      ( ("ne", dummySpanLimited)
      , Left $ Diagnostics
        [syntaxErr (mapSpan (+ 1) dummySpan) "unexpected character `e`"]
      )

    it "should work on try backtrack with alternate" $ testParser
      "ne"
      (try (string "nw") <|> try (string "ne"))
      (("", mapSpanLimited (+ 2) dummySpanLimited), Right "ne")

  describe "Syntax.ParserGeneric.optional" $ do
    it "should return nothing if optional" $ testParser
      "123"
      (pure (,) <*> optional (string "hi") <*> string "123")
      (("", mapSpanLimited (+ 3) dummySpanLimited), Right (Nothing, "123"))
    it "should return nothing is eof" $ testParser
      ""
      (pure (,) <*> optional (string "hi") <*> optional (string "123"))
      (("", dummySpanLimited), Right (Nothing, Nothing))

  describe "Syntax.ParserGeneric.string" $ do
    it "should fail on alternate without try backtrack" $ testParser
      "ne"
      (string "nw" <|> string "ne")
      ( ("e", mapSpanLimited (+ 1) dummySpanLimited)
      , Left $ Diagnostics
        [syntaxErr (mapSpan (+ 1) dummySpan) "unexpected character `e`"]
      )

  describe "Syntax.ParserGeneric.manyParser" $ do
    it "should parse no characters" $ testParser
      "000"
      (manyParser (match 'a'))
      (("000", dummySpanLimited), Right [])
    it "should parse multiple" $ testParser
      "000"
      (manyParser (match '0'))
      (("", mapSpanLimited (+ 3) dummySpanLimited), Right "000")

    it "should not fail multiple after 1" $ testParser
      "00b"
      (manyParser (match '0'))
      (("b", mapSpanLimited (+ 2) dummySpanLimited), Right "00")

  describe "Syntax.ParserGeneric.someParser" $ do
    it "should fail if there are no matches" $ testParser
      "b"
      (someParser (match '0'))
      ( ("b", dummySpanLimited)
      , Left $ Diagnostics [syntaxErr dummySpan "unexpected character `b`"]
      )
    it "should parse if there is 1 match" $ testParser
      "0b"
      (someParser (match '0'))
      (("b", mapSpanLimited (+ 1) dummySpanLimited), Right "0")
    it "should parse if there is more than 1 match" $ testParser
      "00000b"
      (someParser (match '0'))
      (("b", mapSpanLimited (+ 5) dummySpanLimited), Right "00000")

  describe "Syntax.ParserGeneric.oneOf" $ do
    it "should choose between options with multiple options" $ testParser
      "acba"
      (manyParser (oneOf ['a', 'b', 'c']))
      (("", mapSpanLimited (+ 4) dummySpanLimited), Right "acba")
    it "should choose between options with one option" $ testParser
      "a"
      (oneOf ['a'])
      (("", mapSpanLimited (+ 1) dummySpanLimited), Right 'a')

  describe "Syntax.ParserGeneric.string" $ do
    it "should match a string exactly" $ testParser
      "test_keyword"
      (string "test_keyword")
      (("", mapSpanLimited (+ 12) dummySpanLimited), Right "test_keyword")

