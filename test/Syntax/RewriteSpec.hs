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
      $          getOpRules [OpDefS (Operator "$" d) (Binary 10 LeftA) d]
      `shouldBe` Rules M.empty (M.singleton "$" (100, LeftA)) M.empty
    it "should return prefix operator statements"
      $          getOpRules [OpDefS (Operator "$" d) (Prefix 100) d]
      `shouldBe` Rules (M.singleton "$" 1000) M.empty M.empty
    it "should return postfix operator statements"
      $          getOpRules [OpDefS (Operator "$" d) (Postfix 100) d]
      `shouldBe` Rules M.empty M.empty (M.singleton "$" 1000)
    it "should return many operators"
      $          getOpRules
                   [ OpDefS (Operator "$" d) (Postfix 15) d
                   , TraitS (Ident "hi" d) [] [] d   -- Should ignore this
                   , OpDefS (Operator "$" d) (Binary 15 LeftA) d
                   , OpDefS (Operator "$" d) (Prefix 15)       d
                   , TraitS (Ident "hi" d) [] [] d   -- Should ignore this
                   , TraitS (Ident "hi" d) [] [] d   -- Should ignore this
                   , TraitS (Ident "hi" d) [] [] d   -- Should ignore this
                   , OpDefS (Operator "<$>" d) (Binary 10 RightA) d
                   , OpDefS (Operator "<$>" d) (Binary 5 RightA)  d
                   , TraitS (Ident "hi" d) [] [] d   -- Should ignore this
                   , OpDefS (Operator "." d) (Binary 50 RightA) d
                   ]
      `shouldBe` Rules
                   { prefixOps  = M.fromList [("$", 150)]
                   , binaryOps  =
                     M.fromList
                       [ ("$"  , (150, LeftA))
                       , ("."  , (500, RightA))
                       , ("<$>", (50, RightA))
                       ]
                   , postfixOps = M.fromList [("$", 150)]
                   }
    it "should rewrite basic binary operators"
      $          rewriteExpr
                   Rules
                     { prefixOps  = M.empty
                     , binaryOps  = M.fromList
                       [("+", (30, LeftA)), ("*", (40, LeftA)), ("<$>", (20, LeftA))]
                     , postfixOps = M.empty
                     }
                   (ExprList
                     [ OtherTok
                       (LiteralE (IntegerL 1 (Span (SourceId 1) 8 9))
                                 (Span (SourceId 1) 8 9)
                       )
                     , OpTok (Operator "+" (Span (SourceId 1) 9 10))
                     , OtherTok
                       (LiteralE (IntegerL 2 (Span (SourceId 1) 10 11))
                                 (Span (SourceId 1) 10 11)
                       )
                     , OpTok (Operator "+" (Span (SourceId 1) 11 12))
                     , OtherTok
                       (LiteralE (IntegerL 3 (Span (SourceId 1) 12 13))
                                 (Span (SourceId 1) 12 13)
                       )
                     , OpTok (Operator "*" (Span (SourceId 1) 13 14))
                     , OtherTok
                       (LiteralE (IntegerL 4 (Span (SourceId 1) 14 15))
                                 (Span (SourceId 1) 14 15)
                       )
                     ]
                     (Span (SourceId 1) 8 15)
                   )
      `shouldBe` (Right
                   (Just
                     (OpE
                       (BinOp
                         (Operator "+" (Span (SourceId 1) 11 12))
                         (OpE
                           (BinOp
                             (Operator "+" (Span (SourceId 1) 9 10))
                             (LiteralE (IntegerL 1 (Span (SourceId 1) 8 9))
                                       (Span (SourceId 1) 8 9)
                             )
                             (LiteralE (IntegerL 2 (Span (SourceId 1) 10 11))
                                       (Span (SourceId 1) 10 11)
                             )
                           )
                           (Span (SourceId 1) 8 11)
                         )
                         (OpE
                           (BinOp
                             (Operator "*" (Span (SourceId 1) 13 14))
                             (LiteralE (IntegerL 3 (Span (SourceId 1) 12 13))
                                       (Span (SourceId 1) 12 13)
                             )
                             (LiteralE (IntegerL 4 (Span (SourceId 1) 14 15))
                                       (Span (SourceId 1) 14 15)
                             )
                           )
                           (Span (SourceId 1) 12 15)
                         )
                       )
                       (Span (SourceId 1) 8 15)
                     )
                   )
                 )
