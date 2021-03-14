module Syntax.ParserSpec where
import           Test.Hspec                     ( describe
                                                , it
                                                , shouldBe
                                                , Spec
                                                , Expectation
                                                )
import           TestUtil
import           Syntax.Parser
import           Syntax.ParserGeneric
import           Syntax.Ast
import qualified Syntax.Token                  as T
import           Sources
import           Diagnostics

spec :: Spec
spec = do
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
