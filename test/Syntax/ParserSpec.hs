module Syntax.ParserSpec where

import           Test.Hspec                     ( describe
                                                , it
                                                , shouldBe
                                                , Spec
                                                )

import           Control.Monad.Except

import           Sources
import           Syntax.Ast
import           Syntax.Parser
import           Diagnostics

sid = SourceId 0
sn = Span sid

spec :: Spec
spec = do
  describe "Syntax.Parser.parseBlock" $ do
    it "should return empty list on empty input"
      $          parseBlock sid ""
      `shouldBe` Right []
    it "should parse a simple declaration"
      $          parseBlock sid "dec fold' : Int"
      `shouldBe` Right
                   [ DeclarationS
                       ( Declaration
                           (Ident "fold'" $ sn 4 9)
                           (NamespaceType
                             (Namespace [Ident "Int" $ sn 12 15] $ sn 12 15)
                             (sn 12 15)
                           )
                       $ sn 0 15
                       )
                       (sn 0 15)
                   ]
  describe "Syntax.Parser.parseType" $ do
    it "should parse an empty tuple" $ parseType sid "()" `shouldBe` Right
      (TupleType [] (sn 0 2))
    it "should parse an empty tuple with comma"
      $          parseType sid "(,)"
      `shouldBe` Right (TupleType [] (sn 0 3))
    it "should parse a tuple" $ parseType sid "(a,)" `shouldBe` Right
      (TupleType
        [NamespaceType (Namespace [Ident "a" $ sn 1 2] $ sn 1 2) (sn 1 2)]
        (sn 0 4)
      )
    it "should parse two tuple items without trailing comma"
      $          parseType sid "(a,b)"
      `shouldBe` Right
                   (TupleType
                     [ NamespaceType (Namespace [Ident "a" $ sn 1 2] $ sn 1 2)
                                     (sn 1 2)
                     , NamespaceType (Namespace [Ident "b" $ sn 3 4] $ sn 3 4)
                                     (sn 3 4)
                     ]
                     (sn 0 5)
                   )
    it "should parse two tuple items with trailing comma"
      $          parseType sid "(a,b,)"
      `shouldBe` Right
                   (TupleType
                     [ NamespaceType (Namespace [Ident "a" $ sn 1 2] $ sn 1 2)
                                     (sn 1 2)
                     , NamespaceType (Namespace [Ident "b" $ sn 3 4] $ sn 3 4)
                                     (sn 3 4)
                     ]
                     (sn 0 6)
                   )
    it "should parse a tuple with many items"
      $          parseType sid "(a,b,c,d,e)"
      `shouldBe` Right
                   (TupleType
                     [ NamespaceType (Namespace [Ident "a" $ sn 1 2] $ sn 1 2)
                                     (sn 1 2)
                     , NamespaceType (Namespace [Ident "b" $ sn 3 4] $ sn 3 4)
                                     (sn 3 4)
                     , NamespaceType (Namespace [Ident "c" $ sn 5 6] $ sn 5 6)
                                     (sn 5 6)
                     , NamespaceType (Namespace [Ident "d" $ sn 7 8] $ sn 7 8)
                                     (sn 7 8)
                     , NamespaceType
                       (Namespace [Ident "e" $ sn 9 10] $ sn 9 10)
                       (sn 9 10)
                     ]
                     (sn 0 11)
                   )
    it "should parse a function type with multiple"
      $          parseType sid "String -> Int -> Bool"
      `shouldBe` Right
                   (BinOpType
                     (NamespaceType
                       (Namespace [Ident "String" (Span (SourceId 0) 0 6)]
                                  (Span (SourceId 0) 0 6)
                       )
                       (Span (SourceId 0) 0 6)
                     )
                     (Operator "->" (Span (SourceId 0) 7 9))
                     (BinOpType
                       (NamespaceType
                         (Namespace [Ident "Int" (Span (SourceId 0) 10 13)]
                                    (Span (SourceId 0) 10 13)
                         )
                         (Span (SourceId 0) 10 13)
                       )
                       (Operator "->" (Span (SourceId 0) 14 16))
                       (NamespaceType
                         (Namespace [Ident "Bool" (Span (SourceId 0) 17 21)]
                                    (Span (SourceId 0) 17 21)
                         )
                         (Span (SourceId 0) 17 21)
                       )
                       (Span (SourceId 0) 10 21)
                     )
                     (Span (SourceId 0) 0 21)
                   )
    it "should parse a type application"
      $          parseType sid "Option Int Float"
      `shouldBe` Right
                   (BinOpType
                     (BinOpType
                       (NamespaceType
                         (Namespace [Ident "Option" $ sn 0 6] $ sn 0 6)
                         (sn 0 6)
                       )
                       (Operator "application" $ sn 6 7)
                       (NamespaceType
                         (Namespace [Ident "Int" $ sn 7 10] $ sn 7 10)
                         (sn 7 10)
                       )
                       (sn 0 10)
                     )
                     (Operator "application" $ sn 10 11)
                     (NamespaceType
                       (Namespace [Ident "Float" $ sn 11 16] $ sn 11 16)
                       (sn 11 16)
                     )
                     (sn 0 16)
                   )
    it "should parse a type application with parens"
      $          parseType sid "Option (Int Float)"
      `shouldBe` Right
                   (BinOpType
                     (NamespaceType
                       (Namespace [Ident "Option" $ sn 0 6] $ sn 0 6)
                       (sn 0 6)
                     )
                     (Operator "application" $ sn 6 7)
                     (BinOpType
                       (NamespaceType
                         (Namespace [Ident "Int" $ sn 8 11] $ sn 8 11)
                         (sn 8 11)
                       )
                       (Operator "application" $ sn 11 12)
                       (NamespaceType
                         (Namespace [Ident "Float" $ sn 12 17] $ sn 12 17)
                         (sn 12 17)
                       )
                       (sn 7 18)
                     )
                     (sn 0 18)
                   )
    it "should parse a function type with applications"
      $          parseType sid "Option Int -> Int"
      `shouldBe` Right
                   (BinOpType
                     (BinOpType
                       (NamespaceType
                         (Namespace [Ident "Option" $ sn 0 6] $ sn 0 6)
                         (sn 0 6)
                       )
                       (Operator "application" $ sn 6 7)
                       (NamespaceType
                         (Namespace [Ident "Int" $ sn 7 10] $ sn 7 10)
                         (sn 7 10)
                       )
                       (sn 0 10)
                     )
                     (Operator "->" $ sn 11 13)
                     (NamespaceType
                       (Namespace [Ident "Int" $ sn 14 17] $ sn 14 17)
                       (sn 14 17)
                     )
                     (sn 0 17)
                   )
    it "should parse a function type with parens properly"
      $          parseType sid "Option (Int -> Int)"
      `shouldBe` Right
                   (BinOpType
                     (NamespaceType
                       (Namespace [Ident "Option" $ sn 0 6] $ sn 0 6)
                       (sn 0 6)
                     )
                     (Operator "application" $ sn 6 7)
                     (BinOpType
                       (NamespaceType
                         (Namespace [Ident "Int" $ sn 8 11] $ sn 8 11)
                         (sn 8 11)
                       )
                       (Operator "->" $ sn 12 14)
                       (NamespaceType
                         (Namespace [Ident "Int" $ sn 15 18] $ sn 15 18)
                         (sn 15 18)
                       )
                       (sn 7 19)
                     )
                     (sn 0 19)
                   )
    it "should parse a more complex type"
      $ parseType sid "Option (Int -> Int) -> (Option Int -> Int) -> Int -> Int"
      `shouldBe` Right
                   (BinOpType
                     (BinOpType
                       (NamespaceType
                         (Namespace [Ident "Option" (Span (SourceId 0) 0 6)]
                                    (Span (SourceId 0) 0 6)
                         )
                         (Span (SourceId 0) 0 6)
                       )
                       (Operator "application" (Span (SourceId 0) 6 7))
                       (BinOpType
                         (NamespaceType
                           (Namespace [Ident "Int" (Span (SourceId 0) 8 11)]
                                      (Span (SourceId 0) 8 11)
                           )
                           (Span (SourceId 0) 8 11)
                         )
                         (Operator "->" (Span (SourceId 0) 12 14))
                         (NamespaceType
                           (Namespace [Ident "Int" (Span (SourceId 0) 15 18)]
                                      (Span (SourceId 0) 15 18)
                           )
                           (Span (SourceId 0) 15 18)
                         )
                         (Span (SourceId 0) 7 19)
                       )
                       (Span (SourceId 0) 0 19)
                     )
                     (Operator "->" (Span (SourceId 0) 20 22))
                     (BinOpType
                       (BinOpType
                         (BinOpType
                           (NamespaceType
                             (Namespace
                               [Ident "Option" (Span (SourceId 0) 24 30)]
                               (Span (SourceId 0) 24 30)
                             )
                             (Span (SourceId 0) 24 30)
                           )
                           (Operator "application" (Span (SourceId 0) 30 31))
                           (NamespaceType
                             (Namespace
                               [Ident "Int" (Span (SourceId 0) 31 34)]
                               (Span (SourceId 0) 31 34)
                             )
                             (Span (SourceId 0) 31 34)
                           )
                           (Span (SourceId 0) 24 34)
                         )
                         (Operator "->" (Span (SourceId 0) 35 37))
                         (NamespaceType
                           (Namespace [Ident "Int" (Span (SourceId 0) 38 41)]
                                      (Span (SourceId 0) 38 41)
                           )
                           (Span (SourceId 0) 38 41)
                         )
                         (Span (SourceId 0) 23 42)
                       )
                       (Operator "->" (Span (SourceId 0) 43 45))
                       (BinOpType
                         (NamespaceType
                           (Namespace [Ident "Int" (Span (SourceId 0) 46 49)]
                                      (Span (SourceId 0) 46 49)
                           )
                           (Span (SourceId 0) 46 49)
                         )
                         (Operator "->" (Span (SourceId 0) 50 52))
                         (NamespaceType
                           (Namespace [Ident "Int" (Span (SourceId 0) 53 56)]
                                      (Span (SourceId 0) 53 56)
                           )
                           (Span (SourceId 0) 53 56)
                         )
                         (Span (SourceId 0) 46 56)
                       )
                       (Span (SourceId 0) 23 56)
                     )
                     (Span (SourceId 0) 0 56)
                   )
  describe "Syntax.Parser.parseExpr" $ do
    it "should parse a literal" $ parseExpr sid "10" `shouldBe` Right
      (LiteralE (IntegerL 10 (sn 0 2)) (sn 0 2))
    it "should parse a function application"
      $          parseExpr sid "1 2"
      `shouldBe` Right
                   (BinOpE (LiteralE (IntegerL 1 (sn 0 1)) (sn 0 1))
                           (Operator "application" $ sn 1 2)
                           (LiteralE (IntegerL 2 (sn 2 3)) (sn 2 3))
                           (sn 0 3)
                   )
    it "should parse a function application with multiple"
      $          parseExpr sid "1 2 3"
      `shouldBe` Right
                   (BinOpE
                     (BinOpE (LiteralE (IntegerL 1 (sn 0 1)) (sn 0 1))
                             (Operator "application" $ sn 1 2)
                             (LiteralE (IntegerL 2 (sn 2 3)) (sn 2 3))
                             (sn 0 3)
                     )
                     (Operator "application" $ sn 3 4)
                     (LiteralE (IntegerL 3 (sn 4 5)) (sn 4 5))
                     (sn 0 5)
                   )
    it "should parse a function application with multiple and grouping"
      $          parseExpr sid "1 (2 3)"
      `shouldBe` Right
                   (BinOpE
                     (LiteralE (IntegerL 1 (sn 0 1)) (sn 0 1))
                     (Operator "application" $ sn 1 2)
                     (GroupedE
                       (BinOpE (LiteralE (IntegerL 2 (sn 3 4)) (sn 3 4))
                               (Operator "application" $ sn 4 5)
                               (LiteralE (IntegerL 3 (sn 5 6)) (sn 5 6))
                               (sn 3 6)
                       )
                       (sn 2 7)
                     )
                     (sn 0 7)
                   )
    it "should parse an empty tuple" $ parseExpr sid "()" `shouldBe` Right
      (TupleE [] $ sn 0 2)
    it "should parse an empty tuple with comma"
      $          parseExpr sid "(,)"
      `shouldBe` Right (TupleE [] $ sn 0 3)
    it "should parse a tuple with 1 item"
      $          parseExpr sid "(10,)"
      `shouldBe` Right
                   (TupleE [LiteralE (IntegerL 10 (sn 1 3)) (sn 1 3)] $ sn 0 5)
    it "should parse a tuple with trailing comma"
      $          parseExpr sid "(10,20,)"
      `shouldBe` Right
                   ( TupleE
                       [ LiteralE (IntegerL 10 (sn 1 3)) (sn 1 3)
                       , LiteralE (IntegerL 20 (sn 4 6)) (sn 4 6)
                       ]
                   $ sn 0 8
                   )
    it "should parse a tuple without trailing comma"
      $          parseExpr sid "(10,20)"
      `shouldBe` Right
                   ( TupleE
                       [ LiteralE (IntegerL 10 (sn 1 3)) (sn 1 3)
                       , LiteralE (IntegerL 20 (sn 4 6)) (sn 4 6)
                       ]
                   $ sn 0 7
                   )
    it "should parse a tuple with many items"
      $          parseExpr sid "(10,20,30,40,50)"
      `shouldBe` Right
                   ( TupleE
                       [ LiteralE (IntegerL 10 (sn 1 3))   (sn 1 3)
                       , LiteralE (IntegerL 20 (sn 4 6))   (sn 4 6)
                       , LiteralE (IntegerL 30 (sn 7 9))   (sn 7 9)
                       , LiteralE (IntegerL 40 (sn 10 12)) (sn 10 12)
                       , LiteralE (IntegerL 50 (sn 13 15)) (sn 13 15)
                       ]
                   $ sn 0 16
                   )
    it "should parse an operator" $ parseExpr sid "10 + 20" `shouldBe` Right
      ( BinOpE (LiteralE (IntegerL 10 (sn 0 2)) (sn 0 2))
               (Operator "+" $ sn 3 4)
               (LiteralE (IntegerL 20 (sn 5 7)) (sn 5 7))
      $ sn 0 7
      )
    it "should parse multiple operators"
      $          parseExpr sid "10 + 20 ++ 30"
      `shouldBe` Right
                   ( BinOpE
                       ( BinOpE (LiteralE (IntegerL 10 (sn 0 2)) (sn 0 2))
                                (Operator "+" $ sn 3 4)
                                (LiteralE (IntegerL 20 (sn 5 7)) (sn 5 7))
                       $ sn 0 7
                       )
                       (Operator "++" $ sn 8 10)
                       (LiteralE (IntegerL 30 (sn 11 13)) (sn 11 13))
                   $ sn 0 13
                   )
    it "should parse an if statement"
      $          parseExpr sid "if 30 { 10 } else { 20 }"
      `shouldBe` Right
                   (CondE
                     ( LiteralE (IntegerL 30 (sn 3 5))  (sn 3 5)
                     , LiteralE (IntegerL 10 (sn 8 10)) (sn 8 10)
                     )
                     []
                     (LiteralE (IntegerL 20 (sn 20 22)) (sn 20 22))
                     (sn 0 24)
                   )
    it "should parse an if statement with `elif` clause"
      $          parseExpr sid "if 30 { 10 } elif 40 { 50 } else { 20 }"
      `shouldBe` Right
                   (CondE
                     ( LiteralE (IntegerL 30 (sn 3 5))  (sn 3 5)
                     , LiteralE (IntegerL 10 (sn 8 10)) (sn 8 10)
                     )
                     [ ( LiteralE (IntegerL 40 (sn 18 20)) (sn 18 20)
                       , LiteralE (IntegerL 50 (sn 23 25)) (sn 23 25)
                       )
                     ]
                     (LiteralE (IntegerL 20 (sn 35 37)) (sn 35 37))
                     (sn 0 39)
                   )
    it "should parse an if statement with multiple `elif` clauses"
      $ parseExpr sid "if 30 { 10 } elif 40 { 50 } elif 60 { 70 } else { 20 }"
      `shouldBe` Right
                   (CondE
                     ( LiteralE (IntegerL 30 (sn 3 5))  (sn 3 5)
                     , LiteralE (IntegerL 10 (sn 8 10)) (sn 8 10)
                     )
                     [ ( LiteralE (IntegerL 40 (sn 18 20)) (sn 18 20)
                       , LiteralE (IntegerL 50 (sn 23 25)) (sn 23 25)
                       )
                     , ( LiteralE (IntegerL 60 (sn 33 35)) (sn 33 35)
                       , LiteralE (IntegerL 70 (sn 38 40)) (sn 38 40)
                       )
                     ]
                     (LiteralE (IntegerL 20 (sn 50 52)) (sn 50 52))
                     (sn 0 54)
                   )
    it "should parse a basic let in binding"
      $          parseExpr sid "let x = 10 in { 30 }"
      `shouldBe` Right
                   (LetInE
                     [ Binding
                         [ BindP (Ident "x" (Span (SourceId 0) 4 5))
                                 (Span (SourceId 0) 4 5)
                         ]
                         (LiteralE (IntegerL 10 (Span (SourceId 0) 8 10))
                                   (Span (SourceId 0) 8 10)
                         )
                         (Span (SourceId 0) 4 10)
                     ]
                     (LiteralE (IntegerL 30 (Span (SourceId 0) 16 18))
                               (Span (SourceId 0) 16 18)
                     )
                     (Span (SourceId 0) 0 20)
                   )
    it "should parse multiple patterns"
      $          parseExpr sid "let x y = 10 in { 30 40 }"
      `shouldBe` Right
                   (LetInE
                     [ Binding
                         [ BindP (Ident "x" (Span (SourceId 0) 4 5))
                                 (Span (SourceId 0) 4 5)
                         , BindP (Ident "y" (Span (SourceId 0) 6 7))
                                 (Span (SourceId 0) 6 7)
                         ]
                         (LiteralE (IntegerL 10 (Span (SourceId 0) 10 12))
                                   (Span (SourceId 0) 10 12)
                         )
                         (Span (SourceId 0) 4 12)
                     ]
                     (BinOpE
                       (LiteralE (IntegerL 30 (Span (SourceId 0) 18 20))
                                 (Span (SourceId 0) 18 20)
                       )
                       (Operator "application" (Span (SourceId 0) 20 21))
                       (LiteralE (IntegerL 40 (Span (SourceId 0) 21 23))
                                 (Span (SourceId 0) 21 23)
                       )
                       (Span (SourceId 0) 18 23)
                     )
                     (Span (SourceId 0) 0 25)
                   )

    it "should parse a single pattern with parens"
      $          parseExpr sid "let (x y) = 10 in { 30 }"
      `shouldBe` Right
                   (LetInE
                     [ Binding
                         [ VariantP
                             (Namespace [Ident "x" (Span (SourceId 0) 5 6)]
                                        (Span (SourceId 0) 5 6)
                             )
                             (BindP (Ident "y" (Span (SourceId 0) 7 8))
                                    (Span (SourceId 0) 7 8)
                             )
                             (Span (SourceId 0) 4 9)
                         ]
                         (LiteralE (IntegerL 10 (Span (SourceId 0) 12 14))
                                   (Span (SourceId 0) 12 14)
                         )
                         (Span (SourceId 0) 4 14)
                     ]
                     (LiteralE (IntegerL 30 (Span (SourceId 0) 20 22))
                               (Span (SourceId 0) 20 22)
                     )
                     (Span (SourceId 0) 0 24)
                   )
    it "should parse multiple values with operator"
      $          parseExpr sid "let (Some x~xs) = 10 in { 30 }"
      `shouldBe` Right
                   (LetInE
                     [ Binding
                         [ VariantP
                             (Namespace [Ident "Some" (Span (SourceId 0) 5 9)]
                                        (Span (SourceId 0) 5 9)
                             )
                             (CustomP
                               (BindP (Ident "x" (Span (SourceId 0) 10 11))
                                      (Span (SourceId 0) 10 11)
                               )
                               (Operator "~" (Span (SourceId 0) 11 12))
                               (BindP (Ident "xs" (Span (SourceId 0) 12 14))
                                      (Span (SourceId 0) 12 14)
                               )
                               (Span (SourceId 0) 10 14)
                             )
                             (Span (SourceId 0) 4 15)
                         ]
                         (LiteralE (IntegerL 10 (Span (SourceId 0) 18 20))
                                   (Span (SourceId 0) 18 20)
                         )
                         (Span (SourceId 0) 4 20)
                     ]
                     (LiteralE (IntegerL 30 (Span (SourceId 0) 26 28))
                               (Span (SourceId 0) 26 28)
                     )
                     (Span (SourceId 0) 0 30)
                   )
