module Syntax.ParserSpec where

import Control.Applicative
    ( Alternative((<|>), many, some), optional )
import Test.Hspec ( describe, it, shouldBe, Spec, Expectation )
import Test.QuickCheck ()
import           Control.Exception              ( evaluate )
import Syntax.Parser
    ( (<||>),
      char,
      dummySpanLimited,
      getSpan',
      ident,
      manyParser,
      mapSpanLimited,
      namespace,
      number,
      oneOf,
      satisfy,
      someParser,
      string,
      syntaxErr,
      try,
      Parser(P),
      ParserContext,
      SpanLimited(SpanLimited) )
import Syntax.Ast ( Ident(Ident), Namespace(Namespace) )
import Diagnostics ( Diagnostics(..) )
import Sources
    ( Span(Span), SourceId(SourceId), dummySpan, mapSpan )

{-# ANN spec "HLint: ignore Use <$>" #-}

testParser
  :: (Show a, Eq a)
  => String
  -> Parser a
  -> (ParserContext, Either Diagnostics a)
  -> Expectation
testParser source (P fn) x = fn source dummySpanLimited `shouldBe` x

spec :: Spec
spec = do
  describe "Syntax.Parser.Parser" $ do
    it "should map properly" $ testParser
      "a"
      (fmap (\a -> a : ['b']) (char 'a'))
      (("", mapSpanLimited (+ 1) dummySpanLimited), Right "ab")
    it "should apply properly" $ testParser
      "a"
      (pure (\a -> a : ['b']) <*> char 'a')
      (("", mapSpanLimited (+ 1) dummySpanLimited), Right "ab")
    it "should combine properly" $ testParser
      "aba123"
      (many (oneOf "ab") <||> number)
      (("", mapSpanLimited (+ 6) dummySpanLimited), Right "123")
    it "should work monadically"
      $ let combined = do
              char <- many (oneOf ['a', 'b', 'c'])
              num  <- number
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
              num  <- number
              return [char, num]
        in  testParser
              "10cabc10"
              combined
              ( ("10cabc10", dummySpanLimited)
              , Left
                $ Diagnostics [syntaxErr dummySpan "unexpected character `1`"]
              )

  describe "Syntax.Parser.getSpan" $ do
    it "should return the span" $ testParser
      "abc1"
      (getSpan' ident)
      ( ("", mapSpanLimited (+ 4) dummySpanLimited)
      , Right $ SpanLimited 4 (SourceId 0)
      )

  describe "Syntax.Parser.satisfy" $ do
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
  describe "Syntax.Parser.try" $ do
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

  describe "Syntax.Parser.optional" $ do
    it "should return nothing if optional" $ testParser
      "123"
      (pure (,) <*> optional ident <*> number)
      (("", mapSpanLimited (+ 3) dummySpanLimited), Right (Nothing, "123"))
    it "should return nothing is eof" $ testParser
      ""
      (pure (,) <*> optional ident <*> optional number)
      (("", dummySpanLimited), Right (Nothing, Nothing))

  describe "Syntax.Parser.string" $ do
    it "should fail on alternate without try backtrack" $ testParser
      "ne"
      (string "nw" <|> string "ne")
      ( ("e", mapSpanLimited (+ 1) dummySpanLimited)
      , Left $ Diagnostics
        [syntaxErr (mapSpan (+ 1) dummySpan) "unexpected character `e`"]
      )

  describe "Syntax.Parser.manyParser" $ do
    it "should parse no characters" $ testParser
      "000"
      (manyParser (char 'a'))
      (("000", dummySpanLimited), Right [])
    it "should parse multiple" $ testParser
      "000"
      (manyParser (char '0'))
      (("", mapSpanLimited (+ 3) dummySpanLimited), Right "000")

    it "should not fail multiple after 1" $ testParser
      "00b"
      (manyParser (char '0'))
      (("b", mapSpanLimited (+ 2) dummySpanLimited), Right "00")

  describe "Syntax.Parser.someParser" $ do
    it "should fail if there are no matches" $ testParser
      "b"
      (someParser (char '0'))
      ( ("b", dummySpanLimited)
      , Left $ Diagnostics [syntaxErr dummySpan "unexpected character `b`"]
      )
    it "should parse if there is 1 match" $ testParser
      "0b"
      (someParser (char '0'))
      (("b", mapSpanLimited (+ 1) dummySpanLimited), Right "0")
    it "should parse if there is more than 1 match" $ testParser
      "00000b"
      (someParser (char '0'))
      (("b", mapSpanLimited (+ 5) dummySpanLimited), Right "00000")

  describe "Syntax.Parser.oneOf" $ do
    it "should choose between options with multiple options" $ testParser
      "acba"
      (manyParser (oneOf ['a', 'b', 'c']))
      (("", mapSpanLimited (+ 4) dummySpanLimited), Right "acba")
    it "should choose between options with one option" $ testParser
      "a"
      (oneOf ['a'])
      (("", mapSpanLimited (+ 1) dummySpanLimited), Right 'a')

  describe "Syntax.Parser.string" $ do
    it "should match a string exactly" $ testParser
      "test_keyword"
      (string "test_keyword")
      (("", mapSpanLimited (+ 12) dummySpanLimited), Right "test_keyword")

  describe "Syntax.Parser.number" $ do
    it "should parse the number" $ testParser
      "123"
      number
      (("", mapSpanLimited (+ 3) dummySpanLimited), Right "123")
    it "should fail if not a number" $ testParser
      "abc"
      number
      ( ("abc", dummySpanLimited)
      , Left $ Diagnostics [syntaxErr dummySpan "unexpected character `a`"]
      )

  describe "Syntax.Parser.ident" $ do
    it "should parse ident starting with `_`" $ testParser
      "_a123"
      ident
      ( ("", mapSpanLimited (+ 5) dummySpanLimited)
      , Right $ Ident "_a123" (Span 0 5 $ SourceId 0)
      )
    it "should parse ident starting with `a`" $ testParser
      "a123"
      ident
      ( ("", mapSpanLimited (+ 4) dummySpanLimited)
      , Right $ Ident "a123" (Span 0 4 $ SourceId 0)
      )
    it "should fail ident starting with `1`" $ testParser
      "1a23"
      ident
      ( ("1a23", dummySpanLimited)
      , Left $ Diagnostics [syntaxErr dummySpan "unexpected character `1`"]
      )

  describe "Syntax.Parser.namespace" $ do
    it "should parse a namespace with one item" $ testParser
      "a123"
      namespace
      ( ("", mapSpanLimited (+ 4) dummySpanLimited)
      , Right $ Namespace [Ident "a123" (Span 0 4 $ SourceId 0)]
                          (Span 0 4 $ SourceId 0)
      )
    it "should parse a namespace with more than one item" $ testParser
      "a123.ba"
      namespace
      ( ("", mapSpanLimited (+ 7) dummySpanLimited)
      , Right $ Namespace
        [ Ident "a123" (Span 0 4 $ SourceId 0)
        , Ident "ba"   (Span 5 7 $ SourceId 0)
        ]
        (Span 0 7 $ SourceId 0)
      )
    it "should error if starting with dot" $ testParser
      ".ba"
      namespace
      ( (".ba", dummySpanLimited)
      , Left $ Diagnostics [syntaxErr dummySpan "unexpected character `.`"]
      )
    it "shouldn't error if ending with dot" $ testParser
      "ba."
      namespace
      ( (".", mapSpanLimited (+ 2) dummySpanLimited)
      , Right
        $ Namespace [Ident "ba" (Span 0 2 $ SourceId 0)] (Span 0 2 $ SourceId 0)
      )
