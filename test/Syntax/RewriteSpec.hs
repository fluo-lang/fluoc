module Syntax.RewriteSpec where

import           Test.Hspec                     ( describe
                                                , it
                                                , shouldBe
                                                , Spec
                                                )
import qualified Data.Map                      as M

import           Sources
import           Syntax.Ast
import           Syntax.Rewrite

sid :: SourceId
sid = SourceId 0
sn :: Int -> Int -> Span
sn = Span sid
d :: Span
d = sn 0 1

spec :: Spec
spec = do
  describe "Syntax.Rewrite.getOpRules" $ do
    it "should return empty on empty statements"
      $          getOpRules []
      `shouldBe` Rules M.empty M.empty M.empty
    it "should return binary operator statements"
      $          getOpRules [OpDefS (Operator "$" d) (OpInfo LeftA Binary 10) d]
      `shouldBe` Rules M.empty (M.singleton "$" $ OpEntry LeftA 10) M.empty
    it "should return prefix operator statements"
      $ getOpRules [OpDefS (Operator "$" d) (OpInfo RightA Prefix 100) d]
      `shouldBe` Rules (M.singleton "$" $ OpEntry RightA 100) M.empty M.empty
    it "should return postfix operator statements"
      $ getOpRules [OpDefS (Operator "$" d) (OpInfo RightA Postfix 100) d]
      `shouldBe` Rules M.empty M.empty (M.singleton "$" $ OpEntry RightA 100)
    it "should return many operators"
      $          getOpRules
                   [ OpDefS (Operator "$" d) (OpInfo RightA Postfix 15) d
                   , TraitS (Ident "hi" d) [] [] d   -- Should ignore this
                   , OpDefS (Operator "$" d)   (OpInfo RightA Binary 15) d
                   , OpDefS (Operator "$" d)   (OpInfo LeftA Prefix 15)  d
                   , TraitS (Ident "hi" d) [] [] d   -- Should ignore this
                   , TraitS (Ident "hi" d) [] [] d   -- Should ignore this
                   , TraitS (Ident "hi" d) [] [] d   -- Should ignore this
                   , OpDefS (Operator "<$>" d) (OpInfo RightA Binary 10) d
                   , OpDefS (Operator "<$>" d) (OpInfo RightA Binary 5) d
                   , TraitS (Ident "hi" d) [] [] d   -- Should ignore this
                   , OpDefS (Operator "." d)   (OpInfo RightA Binary 50) d
                   ]
      `shouldBe` Rules
                   { prefixOps  = M.fromList [("$", OpEntry LeftA 15)]
                   , binaryOps  = M.fromList
                                    [ ("$"  , OpEntry RightA 15)
                                    , ("."  , OpEntry RightA 50)
                                    , ("<$>", OpEntry RightA 5)
                                    ]
                   , postfixOps = M.fromList [("$", OpEntry RightA 15)]
                   }
