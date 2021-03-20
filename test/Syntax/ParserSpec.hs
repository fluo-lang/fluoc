module Syntax.ParserSpec where
import           Test.Hspec                     ( describe
                                                , it
                                                , shouldBe
                                                , Spec
                                                , Expectation
                                                )
import           Syntax.Parser
import           Syntax.ParserGeneric
import           Syntax.Ast
import qualified Syntax.Token                  as T
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

-- New Span
ns = Span (SourceId 0)

spec :: Spec
spec = do
  describe "Syntax.Parser.ty" $ do
    it "should parse an infer type" $ testParser
      [T.Token (T.Ident "_") (Span (SourceId 0) 0 1)]
      ty
      ( ([], mapSpanLimited (+ 1) dummySpanLimited)
      , Right $ Infer (Span (SourceId 0) 0 1)
      )
    it "should parse a namespace type"
      $ let ns =
              [ T.Token (T.Ident "a123") (Span (SourceId 0) 0 4)
              , T.Token (T.Operator "::") (Span (SourceId 0) 4 6)
              , T.Token (T.Ident "ba") (Span (SourceId 0) 6 8)
              ]
        in  testParser
              ns
              ty
              ( ([], mapSpanLimited (+ 8) dummySpanLimited)
              , Right $ NamespaceType
                (Namespace
                  [ Ident "a123" (Span (SourceId 0) 0 4)
                  , Ident "ba"   (Span (SourceId 0) 6 8)
                  ]
                  (Span (SourceId 0) 0 8)
                )
                (Span (SourceId 0) 0 8)
              )
    it "should parse a type application" $ testParser
      [ T.Token (T.Ident "a") (Span (SourceId 0) 0 1)
      , T.Token (T.Ident "b") (Span (SourceId 0) 1 2)
      ]
      ty
      ( ([], mapSpanLimited (+ 2) dummySpanLimited)
      , Right $ TypeApplication
        (Namespace [Ident "a" $ ns 0 1] $ ns 0 1)
        [NamespaceType (Namespace [Ident "b" $ ns 1 2] (ns 1 2)) $ ns 1 2]
        (ns 0 2)
      )
  describe "Syntax.Parser.namespace" $ do
    it "should parse a namespace with one item" $ testParser
      [T.Token (T.Ident "a123") (Span (SourceId 0) 0 4)]
      namespace
      ( ([], mapSpanLimited (+ 4) dummySpanLimited)
      , Right $ Namespace [Ident "a123" (Span (SourceId 0) 0 4)]
                          (Span (SourceId 0) 0 4)
      )
    it "should parse a namespace with more than one item" $ testParser
      [ T.Token (T.Ident "a123") (Span (SourceId 0) 0 4)
      , T.Token (T.Operator "::") (Span (SourceId 0) 4 6)
      , T.Token (T.Ident "ba") (Span (SourceId 0) 6 8)
      ]
      namespace
      ( ([], mapSpanLimited (+ 8) dummySpanLimited)
      , Right $ Namespace
        [ Ident "a123" (Span (SourceId 0) 0 4)
        , Ident "ba"   (Span (SourceId 0) 6 8)
        ]
        (Span (SourceId 0) 0 8)
      )
    it "should error if starting with colon colon" $ testParser
      [ T.Token (T.Operator "::") (Span (SourceId 0) 0 2)
      , T.Token (T.Ident "ba") (Span (SourceId 0) 2 4)
      ]
      namespace
      ( ( [ T.Token (T.Operator "::") (Span (SourceId 0) 0 2)
          , T.Token (T.Ident "ba") (Span (SourceId 0) 2 4)
          ]
        , dummySpanLimited
        )
      , Left $ Diagnostics [syntaxErr dummySpan "unexpected operator `::`"]
      )
    it "shouldn't error if ending with colon colon" $ testParser
      [ T.Token (T.Ident "ba") (Span (SourceId 0) 0 2)
      , T.Token (T.Operator "::") (Span (SourceId 0) 2 4)
      ]
      namespace
      ( ( [T.Token (T.Operator "::") (Span (SourceId 0) 2 4)]
        , mapSpanLimited (+ 2) dummySpanLimited
        )
      , Right
        $ Namespace [Ident "ba" (Span (SourceId 0) 0 2)] (Span (SourceId 0) 0 2)
      )
