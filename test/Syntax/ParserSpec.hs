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
      , T.Token T.Dot (Span (SourceId 0) 4 5)
      , T.Token (T.Ident "ba") (Span (SourceId 0) 5 7)
      ]
      namespace
      ( ([], mapSpanLimited (+ 7) dummySpanLimited)
      , Right $ Namespace
        [ Ident "a123" (Span (SourceId 0) 0 4)
        , Ident "ba"   (Span (SourceId 0) 5 7)
        ]
        (Span (SourceId 0) 0 7)
      )
    it "should error if starting with dot" $ testParser
      [ T.Token T.Dot (Span (SourceId 0) 0 1)
      , T.Token (T.Ident "ba") (Span (SourceId 0) 1 2)
      ]
      namespace
      ( ( [ T.Token T.Dot (Span (SourceId 0) 0 1)
          , T.Token (T.Ident "ba") (Span (SourceId 0) 1 2)
          ]
        , dummySpanLimited
        )
      , Left $ Diagnostics [syntaxErr dummySpan "unexpected token `.`"]
      )
    it "shouldn't error if ending with dot" $ testParser
      [ T.Token (T.Ident "ba") (Span (SourceId 0) 0 2)
      , T.Token T.Dot (Span (SourceId 0) 2 3)
      ]
      namespace
      ( ( [T.Token T.Dot (Span (SourceId 0) 2 3)]
        , mapSpanLimited (+ 2) dummySpanLimited
        )
      , Right
        $ Namespace [Ident "ba" (Span (SourceId 0) 0 2)] (Span (SourceId 0) 0 2)
      )
