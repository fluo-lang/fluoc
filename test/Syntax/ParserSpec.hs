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
import           Syntax.Lexer                   ( getTokens )
import qualified Syntax.Token                  as T
import           Sources
import           Diagnostics
import           Display

testParser
  :: (Show a, Eq a)
  => String
  -> Parser T.Token a
  -> ParserReturn T.Token a
  -> Expectation
testParser source (P fn) x = realRes `shouldBe` expectedRes
 where
  Right toks  = getTokens source
  realRes     = fn toks dummySpanLimited
  expectedRes = x

-- New Span
ns = Span (SourceId 0)

spec :: Spec
spec = do
  describe "Syntax.Parser.declaration" $ do
    it "should parse a basic declaration" $ testParser
      "dec unwrap : Int"
      declaration
      ( ([], mapSpanLimited (+ 16) dummySpanLimited)
      , Right $ Declaration
        (Ident "unwrap" (ns 4 10))
        (NamespaceType $ Namespace [Ident "Int" (ns 13 16)] (ns 13 16))
        (ns 0 16)
      )
  describe "Syntax.Parser.ty" $ do
    it "should parse an infer type" $ testParser
      "_"
      ty
      (([], mapSpanLimited (+ 1) dummySpanLimited), Right $ Infer (ns 0 1))
    it "should parse a namespace type" $ testParser
      "a123::ba"
      ty
      ( ([], mapSpanLimited (+ 8) dummySpanLimited)
      , Right $ NamespaceType
        (Namespace [Ident "a123" (ns 0 4), Ident "ba" (ns 6 8)] (ns 0 8))
      )
    it "should parse a type application" $ testParser
      "a12 b"
      ty
      ( ([], mapSpanLimited (+ 5) dummySpanLimited)
      , Right $ TypeApplication
        (Namespace [Ident "a12" $ ns 0 3] $ ns 0 3)
        [NamespaceType (Namespace [Ident "b" $ ns 4 5] (ns 4 5))]
        (ns 0 5)
      )
  describe "Syntax.Parser.namespace" $ do
    it "should parse a namespace with one item" $ testParser
      "a123"
      namespace
      ( ([], mapSpanLimited (+ 4) dummySpanLimited)
      , Right $ Namespace [Ident "a123" (ns 0 4)] (ns 0 4)
      )
    it "should parse a namespace with spaces" $ testParser
      "a123 :: ba"
      namespace
      ( ([], mapSpanLimited (+ 10) dummySpanLimited)
      , Right
        $ Namespace [Ident "a123" (ns 0 4), Ident "ba" (ns 8 10)] (ns 0 10)
      )
    it "should parse a namespace with more than one item" $ testParser
      "a123::ba"
      namespace
      ( ([], mapSpanLimited (+ 8) dummySpanLimited)
      , Right $ Namespace [Ident "a123" (ns 0 4), Ident "ba" (ns 6 8)] (ns 0 8)
      )
    it "should error if starting with colon colon" $ testParser
      "::ba"
      namespace
      ( ( [T.Token (T.Operator "::") (ns 0 2), T.Token (T.Ident "ba") (ns 2 4)]
        , dummySpanLimited
        )
      , Left $ Diagnostics [syntaxErr dummySpan "unexpected operator `::`"]
      )
    it "shouldn't error if ending with colon colon" $ testParser
      "ba::"
      namespace
      ( ( [T.Token (T.Operator "::") (ns 2 4)]
        , mapSpanLimited (+ 2) dummySpanLimited
        )
      , Right $ Namespace [Ident "ba" (ns 0 2)] (ns 0 2)
      )
