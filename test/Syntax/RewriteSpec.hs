module Syntax.RewriteSpec where

import           Test.Hspec                     ( describe
                                                , it
                                                , shouldBe
                                                , Spec
                                                )
import qualified Data.Map                      as M
import           Control.Monad.Except           ( runExcept )
import           Data.Generics.Uniplate.Operations

import           Sources
import           Syntax.Ast
import           Syntax.Rewrite
import           Errors.Diagnostics             ( Failable )

sid :: SourceId
sid = SourceId 0
sn :: Int -> Int -> Span
sn = Span sid
d :: Span
d = sn 0 1

rewriteExpr' :: Rules -> Expr -> Failable Expr
rewriteExpr' rs e = do
  exprRewrite <- rewriteBiM (rewriteExpr rs) e
  rewriteBiM (rewriteType rs) exprRewrite

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
    it "should parse a function application properly"
      $          runExcept
                   (rewriteExpr
                     Rules
                       { prefixOps  = M.fromList [("-", 30)]
                       , binaryOps  = M.fromList
                                        [("-", (30, LeftA)), (fnAppName, (10, LeftA))]
                       , postfixOps = M.empty
                       }
                     (ExprList
                       [ OtherTok (LiteralE (IntegerL 10 (sn 8 10)) (sn 8 10))
                       , OtherTok (LiteralE (IntegerL 10 (sn 11 12)) (sn 11 12))
                       ]
                       (sn 8 12)
                     )
                   )
      `shouldBe` Right
                   (Just
                     (OpE
                       (BinOp (Operator "<fnapp>" (sn 10 11))
                              (LiteralE (IntegerL 10 (sn 8 10)) (sn 8 10))
                              (LiteralE (IntegerL 10 (sn 11 12)) (sn 11 12))
                       )
                       (sn 8 12)
                     )
                   )
    it "should parse multiple operators chained"
      $          runExcept
                   (rewriteExpr
                     Rules { prefixOps  = M.fromList [("-", 30)]
                           , binaryOps  = M.fromList [("-", (30, LeftA))]
                           , postfixOps = M.empty
                           }
                     (ExprList
                       [ OtherTok (LiteralE (IntegerL 10 (sn 8 10)) (sn 8 10))
                       , OpTok (Operator "-" (sn 11 12))
                       , OpTok (Operator "-" (sn 13 14))
                       , OpTok (Operator "-" (sn 15 16))
                       , OpTok (Operator "-" (sn 17 18))
                       , OtherTok (LiteralE (IntegerL 10 (sn 18 20)) (sn 18 20))
                       ]
                       (sn 8 20)
                     )
                   )
      `shouldBe` Right
                   (Just
                     (OpE
                       (BinOp
                         (Operator "-" (sn 11 12))
                         (LiteralE (IntegerL 10 (sn 8 10)) (sn 8 10))
                         (OpE
                           (PreOp
                             (Operator "-" (sn 13 14))
                             (OpE
                               (PreOp
                                 (Operator "-" (sn 15 16))
                                 (OpE
                                   (PreOp
                                     (Operator "-" (sn 17 18))
                                     (LiteralE (IntegerL 10 (sn 18 20))
                                               (sn 18 20)
                                     )
                                   )
                                   (sn 17 20)
                                 )
                               )
                               (sn 15 20)
                             )
                           )
                           (sn 13 20)
                         )
                       )
                       (sn 8 20)
                     )
                   )
    it "should do binary then prefix if tie in prec"
      $          runExcept
                   (rewriteExpr'
                     Rules { prefixOps  = M.fromList [("-", 30)]
                           , binaryOps  = M.fromList [("-", (30, LeftA))]
                           , postfixOps = M.empty
                           }
                     (ExprList
                       [ OtherTok (LiteralE (IntegerL 10 (sn 8 10)) (sn 8 10))
                       , OpTok (Operator "-" (sn 11 12))
                       , OpTok (Operator "-" (sn 13 14))
                       , OpTok (Operator "-" (sn 15 16))
                       , OpTok (Operator "-" (sn 17 18))
                       , OtherTok (LiteralE (IntegerL 10 (sn 18 20)) (sn 18 20))
                       ]
                       (sn 8 20)
                     )
                   )
      `shouldBe` Right
                   (OpE
                     (BinOp
                       (Operator "-" (sn 11 12))
                       (LiteralE (IntegerL 10 (sn 8 10)) (sn 8 10))
                       (OpE
                         (PreOp
                           (Operator "-" (sn 13 14))
                           (OpE
                             (PreOp
                               (Operator "-" (sn 15 16))
                               (OpE
                                 (PreOp
                                   (Operator "-" (sn 17 18))
                                   (LiteralE (IntegerL 10 (sn 18 20)) (sn 18 20)
                                   )
                                 )
                                 (sn 17 20)
                               )
                             )
                             (sn 15 20)
                           )
                         )
                         (sn 13 20)
                       )
                     )
                     (sn 8 20)
                   )
    it "should rewrite single operator"
      $          runExcept
                   (rewriteExpr'
                     Rules
                       { prefixOps  = M.empty
                       , binaryOps  = M.fromList
                                        [ ("+"      , (30, LeftA))
                                        , ("*"      , (40, LeftA))
                                        , ("<$>"    , (20, LeftA))
                                        , (fnAppName, (10, LeftA))
                                        ]
                       , postfixOps = M.empty
                       }
                     (ExprList
                       [ OtherTok (LiteralE (IntegerL 1 (sn 8 9)) (sn 8 9))
                       , OpTok (Operator "+" (sn 9 10))
                       , OtherTok (LiteralE (IntegerL 2 (sn 10 11)) (sn 10 11))
                       ]
                       (sn 8 11)
                     )
                   )
      `shouldBe` Right
                   (OpE
                     (BinOp (Operator "+" (sn 9 10))
                            (LiteralE (IntegerL 1 (sn 8 9)) (sn 8 9))
                            (LiteralE (IntegerL 2 (sn 10 11)) (sn 10 11))
                     )
                     (sn 8 11)
                   )
    it "should rewrite basic binary operators"
      $          runExcept
                   (rewriteExpr'
                     Rules
                       { prefixOps  = M.empty
                       , binaryOps  = M.fromList
                                        [ ("+"      , (30, LeftA))
                                        , ("*"      , (40, LeftA))
                                        , ("<$>"    , (20, LeftA))
                                        , (fnAppName, (10, LeftA))
                                        ]
                       , postfixOps = M.empty
                       }
                     (ExprList
                       [ OtherTok (LiteralE (IntegerL 1 (sn 8 9)) (sn 8 9))
                       , OpTok (Operator "+" (sn 9 10))
                       , OtherTok (LiteralE (IntegerL 2 (sn 10 11)) (sn 10 11))
                       , OpTok (Operator "+" (sn 11 12))
                       , OtherTok (LiteralE (IntegerL 3 (sn 12 13)) (sn 12 13))
                       , OpTok (Operator "*" (sn 13 14))
                       , OtherTok (LiteralE (IntegerL 4 (sn 14 15)) (sn 14 15))
                       ]
                       (sn 8 15)
                     )
                   )
      `shouldBe` Right
                   (OpE
                     (BinOp
                       (Operator "+" (sn 11 12))
                       (OpE
                         (BinOp (Operator "+" (sn 9 10))
                                (LiteralE (IntegerL 1 (sn 8 9)) (sn 8 9))
                                (LiteralE (IntegerL 2 (sn 10 11)) (sn 10 11))
                         )
                         (sn 8 11)
                       )
                       (OpE
                         (BinOp (Operator "*" (sn 13 14))
                                (LiteralE (IntegerL 3 (sn 12 13)) (sn 12 13))
                                (LiteralE (IntegerL 4 (sn 14 15)) (sn 14 15))
                         )
                         (sn 12 15)
                       )
                     )
                     (sn 8 15)
                   )
    it "should rewrite postfix operators"
      $          runExcept
                   (rewriteExpr'
                     Rules
                       { prefixOps  = M.empty
                       , binaryOps  = M.fromList
                         [(fnAppName, (1, LeftA)), ("!", (4, LeftA)), ("-", (6, LeftA))]
                       , postfixOps = M.singleton "!" 5
                       }
                     (ExprList
                       [ OtherTok
                         (ExprList
                           [ OtherTok
                             (ExprList
                               [ OtherTok
                                 (LiteralE (IntegerL 10 (Span (SourceId 1) 40 42))
                                           (Span (SourceId 1) 40 42)
                                 )
                               , OpTok (Operator "!" (Span (SourceId 1) 42 43))
                               ]
                               (Span (SourceId 1) 40 43)
                             )
                           , OpTok (Operator "-" (Span (SourceId 1) 45 46))
                           , OtherTok
                             (LiteralE (IntegerL 10 (Span (SourceId 1) 47 49))
                                       (Span (SourceId 1) 47 49)
                             )
                           ]
                           (Span (SourceId 1) 40 49)
                         )
                       , OtherTok
                         (LiteralE (IntegerL 10 (Span (SourceId 1) 51 53))
                                   (Span (SourceId 1) 51 53)
                         )
                       ]
                       (Span (SourceId 1) 40 53)
                     )
                   )
      `shouldBe` Right
                   (OpE
                     (BinOp
                       (Operator "<fnapp>" (Span (SourceId 1) 49 51))
                       (OpE
                         (BinOp
                           (Operator "-" (Span (SourceId 1) 45 46))
                           (OpE
                             (PostOp
                               (Operator "!" (Span (SourceId 1) 42 43))
                               (LiteralE
                                 (IntegerL 10 (Span (SourceId 1) 40 42))
                                 (Span (SourceId 1) 40 42)
                               )
                             )
                             (Span (SourceId 1) 40 43)
                           )
                           (LiteralE (IntegerL 10 (Span (SourceId 1) 47 49))
                                     (Span (SourceId 1) 47 49)
                           )
                         )
                         (Span (SourceId 1) 40 49)
                       )
                       (LiteralE (IntegerL 10 (Span (SourceId 1) 51 53))
                                 (Span (SourceId 1) 51 53)
                       )
                     )
                     (Span (SourceId 1) 40 53)
                   )
