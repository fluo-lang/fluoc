module DesugarBeforeSpec where

import           Test.Hspec                     ( it
                                                , shouldBe
                                                , Spec
                                                )

import           Sources
import           Syntax.Ast
import           DesugarBefore

sid :: SourceId
sid = SourceId 0
sn :: Int -> Int -> Span
sn = Span sid
d :: Span
d = sn 0 1

spec :: Spec
spec = do
  it "should remove all opdef statements"
    $          desugarBefore
                 [ OpDefS (Operator "!" (sn 7 8)) (Binary 4 LeftA) (sn 0 23)
                 , OpDefS (Operator "-" (sn 31 32)) (Binary 6 LeftA) (sn 24 47)
                 , OpDefS (Operator "!" (sn 55 56)) (Postfix 5) (sn 48 67)
                 , OpDefS (Operator "<fnapp>" (sn 76 81)) (Binary 1 LeftA) (sn 68 97)
                 ]
    `shouldBe` return []
  it
      "should refactor operator references to function apps and change operator name"
    $          desugarBefore
                 [ OpDefS (Operator "!" (sn 7 8)) (Binary 4 LeftA) (sn 0 23)
                 , OpDefS (Operator "-" (sn 31 32)) (Binary 6 LeftA) (sn 24 47)
                 , OpDefS (Operator "!" (sn 55 56)) (Postfix 5) (sn 48 67)
                 , OpDefS (Operator "<fnapp>" (sn 76 81)) (Binary 1 LeftA) (sn 68 97)
                 , BindingS
                   [ Binding
                       (Just (OpId PostfixF (Operator "!" (sn 104 105))))
                       [ VariableE (Namespace [Ident "x" (sn 111 112)] (sn 111 112))
                                   (sn 111 112)
                       ]
                       (OpE
                         (BinOp
                           (Operator "<fnapp>" (sn 118 119))
                           (VariableE
                             (Namespace [Ident "not" (sn 115 118)] (sn 115 118))
                             (sn 115 118)
                           )
                           (VariableE (Namespace [Ident "x" (sn 119 120)] (sn 119 120))
                                      (sn 119 120)
                           )
                         )
                         (sn 115 120)
                       )
                       (sn 104 120)
                   ]
                   (sn 99 120)
                 , BindingS
                   [ Binding
                       Nothing
                       [ VariableE (Namespace [Ident "a" (sn 125 126)] (sn 125 126))
                                   (sn 125 126)
                       ]
                       (OpE
                         (BinOp
                           (Operator "<fnapp>" (sn 140 142))
                           (OpE
                             (BinOp
                               (Operator "-" (sn 136 137))
                               (OpE
                                 (PostOp
                                   (Operator "!" (sn 133 134))
                                   (LiteralE (IntegerL 10 (sn 131 133)) (sn 131 133))
                                 )
                                 (sn 131 134)
                               )
                               (LiteralE (IntegerL 10 (sn 138 140)) (sn 138 140))
                             )
                             (sn 131 140)
                           )
                           (LiteralE (IntegerL 10 (sn 142 144)) (sn 142 144))
                         )
                         (sn 131 144)
                       )
                       (sn 125 144)
                   ]
                   (sn 121 144)
                 ]
    `shouldBe` return
                 [ BindingS
                   [ Binding
                       (Just (Ident "`!` (postfix)" (sn 104 105)))
                       [ VariableE
                           (Namespace [Ident "x" (sn 111 112)] (sn 111 112))
                           (sn 111 112)
                       ]
                       (FnAppE
                         (VariableE
                           (Namespace [Ident "not" (sn 115 118)] (sn 115 118))
                           (sn 115 118)
                         )
                         (VariableE
                           (Namespace [Ident "x" (sn 119 120)] (sn 119 120))
                           (sn 119 120)
                         )
                         (sn 115 120)
                       )
                       (sn 104 120)
                   ]
                   (sn 99 120)
                 , BindingS
                   [ Binding
                       Nothing
                       [ VariableE
                           (Namespace [Ident "a" (sn 125 126)] (sn 125 126))
                           (sn 125 126)
                       ]
                       (FnAppE
                         (FnAppE
                           (FnAppE
                             (VariableE
                               (Namespace [Ident "`-` (binary)" (sn 136 137)]
                                          (sn 136 137)
                               )
                               (sn 136 137)
                             )
                             (FnAppE
                               (VariableE
                                 (Namespace
                                   [Ident "`!` (postfix)" (sn 133 134)]
                                   (sn 133 134)
                                 )
                                 (sn 133 134)
                               )
                               (LiteralE (IntegerL 10 (sn 131 133)) (sn 131 133)
                               )
                               (sn 131 134)
                             )
                             (sn 131 137)
                           )
                           (LiteralE (IntegerL 10 (sn 138 140)) (sn 138 140))
                           (sn 131 140)
                         )
                         (LiteralE (IntegerL 10 (sn 142 144)) (sn 142 144))
                         (sn 131 144)
                       )
                       (sn 125 144)
                   ]
                   (sn 121 144)
                 ]
