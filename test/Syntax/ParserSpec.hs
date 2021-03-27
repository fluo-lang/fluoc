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
                       (Namespace [Ident "String" (sn 0 6)] (sn 0 6))
                       (sn 0 6)
                     )
                     (Operator "->" (sn 7 9))
                     (BinOpType
                       (NamespaceType
                         (Namespace [Ident "Int" (sn 10 13)] (sn 10 13))
                         (sn 10 13)
                       )
                       (Operator "->" (sn 14 16))
                       (NamespaceType
                         (Namespace [Ident "Bool" (sn 17 21)] (sn 17 21))
                         (sn 17 21)
                       )
                       (sn 10 21)
                     )
                     (sn 0 21)
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
                         (Namespace [Ident "Option" (sn 0 6)] (sn 0 6))
                         (sn 0 6)
                       )
                       (Operator "application" (sn 6 7))
                       (BinOpType
                         (NamespaceType
                           (Namespace [Ident "Int" (sn 8 11)] (sn 8 11))
                           (sn 8 11)
                         )
                         (Operator "->" (sn 12 14))
                         (NamespaceType
                           (Namespace [Ident "Int" (sn 15 18)] (sn 15 18))
                           (sn 15 18)
                         )
                         (sn 7 19)
                       )
                       (sn 0 19)
                     )
                     (Operator "->" (sn 20 22))
                     (BinOpType
                       (BinOpType
                         (BinOpType
                           (NamespaceType
                             (Namespace [Ident "Option" (sn 24 30)] (sn 24 30))
                             (sn 24 30)
                           )
                           (Operator "application" (sn 30 31))
                           (NamespaceType
                             (Namespace [Ident "Int" (sn 31 34)] (sn 31 34))
                             (sn 31 34)
                           )
                           (sn 24 34)
                         )
                         (Operator "->" (sn 35 37))
                         (NamespaceType
                           (Namespace [Ident "Int" (sn 38 41)] (sn 38 41))
                           (sn 38 41)
                         )
                         (sn 23 42)
                       )
                       (Operator "->" (sn 43 45))
                       (BinOpType
                         (NamespaceType
                           (Namespace [Ident "Int" (sn 46 49)] (sn 46 49))
                           (sn 46 49)
                         )
                         (Operator "->" (sn 50 52))
                         (NamespaceType
                           (Namespace [Ident "Int" (sn 53 56)] (sn 53 56))
                           (sn 53 56)
                         )
                         (sn 46 56)
                       )
                       (sn 23 56)
                     )
                     (sn 0 56)
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
                     [ Binding Nothing
                               [BindP (Ident "x" (sn 4 5)) (sn 4 5)]
                               (LiteralE (IntegerL 10 (sn 8 10)) (sn 8 10))
                               (sn 4 10)
                     ]
                     (LiteralE (IntegerL 30 (sn 16 18)) (sn 16 18))
                     (sn 0 20)
                   )
    it "should parse a pattern destructure"
      $          parseExpr sid "let x y = 10 in { 30 40 }"
      `shouldBe` Right
                   (LetInE
                     [ Binding
                         Nothing
                         [ VariantP (Namespace [Ident "x" (sn 4 5)] (sn 4 5))
                                    (BindP (Ident "y" (sn 6 7)) (sn 6 7))
                                    (sn 4 7)
                         ]
                         (LiteralE (IntegerL 10 (sn 10 12)) (sn 10 12))
                         (sn 4 12)
                     ]
                     (BinOpE (LiteralE (IntegerL 30 (sn 18 20)) (sn 18 20))
                             (Operator "application" (sn 20 21))
                             (LiteralE (IntegerL 40 (sn 21 23)) (sn 21 23))
                             (sn 18 23)
                     )
                     (sn 0 25)
                   )

    it "should parse a single pattern with parens"
      $          parseExpr sid "let (x y) = 10 in { 30 }"
      `shouldBe` Right
                   (LetInE
                     [ Binding
                         Nothing
                         [ VariantP (Namespace [Ident "x" (sn 5 6)] (sn 5 6))
                                    (BindP (Ident "y" (sn 7 8)) (sn 7 8))
                                    (sn 4 9)
                         ]
                         (LiteralE (IntegerL 10 (sn 12 14)) (sn 12 14))
                         (sn 4 14)
                     ]
                     (LiteralE (IntegerL 30 (sn 20 22)) (sn 20 22))
                     (sn 0 24)
                   )
    it "should parse multiple values with operator"
      $          parseExpr sid "let (Some x~xs) = 10 in { 30 }"
      `shouldBe` Right
                   (LetInE
                     [ Binding
                         Nothing
                         [ VariantP
                             (Namespace [Ident "Some" (sn 5 9)] (sn 5 9))
                             (CustomP
                               (BindP (Ident "x" (sn 10 11)) (sn 10 11))
                               (Operator "~" (sn 11 12))
                               (BindP (Ident "xs" (sn 12 14)) (sn 12 14))
                               (sn 10 14)
                             )
                             (sn 4 15)
                         ]
                         (LiteralE (IntegerL 10 (sn 18 20)) (sn 18 20))
                         (sn 4 20)
                     ]
                     (LiteralE (IntegerL 30 (sn 26 28)) (sn 26 28))
                     (sn 0 30)
                   )
    it "should parse a function properly"
      $          parseExpr sid "let myfn : (Some x~xs) a = x+a in { 30 }"
      `shouldBe` Right
                   (LetInE
                     [ Binding
                         (Just (Ident "myfn" (sn 4 8)))
                         [ VariantP
                           (Namespace [Ident "Some" (sn 12 16)] (sn 12 16))
                           (CustomP (BindP (Ident "x" (sn 17 18)) (sn 17 18))
                                    (Operator "~" (sn 18 19))
                                    (BindP (Ident "xs" (sn 19 21)) (sn 19 21))
                                    (sn 17 21)
                           )
                           (sn 11 22)
                         , BindP (Ident "a" (sn 23 24)) (sn 23 24)
                         ]
                         (BinOpE
                           (VariableE
                             (Namespace [Ident "x" (sn 27 28)] (sn 27 28))
                             (sn 27 28)
                           )
                           (Operator "+" (sn 28 29))
                           (VariableE
                             (Namespace [Ident "a" (sn 29 30)] (sn 29 30))
                             (sn 29 30)
                           )
                           (sn 27 30)
                         )
                         (sn 4 30)
                     ]
                     (LiteralE (IntegerL 30 (sn 36 38)) (sn 36 38))
                     (sn 0 40)
                   )
    it "should parse a let with multiple clauses"
      $          (head <$> parseBlock
                   sid
                   "let map : f a = a,\n\
                     \    map : f (x~xs) = (f x) ~ (map f xs)"
                 )
      `shouldBe` Right
                   (BindingS
                     [ Binding
                       (Just (Ident "map" (Span (SourceId 0) 4 7)))
                       [ BindP (Ident "f" (Span (SourceId 0) 10 11))
                               (Span (SourceId 0) 10 11)
                       , BindP (Ident "a" (Span (SourceId 0) 12 13))
                               (Span (SourceId 0) 12 13)
                       ]
                       (VariableE
                         (Namespace [Ident "a" (Span (SourceId 0) 16 17)]
                                    (Span (SourceId 0) 16 17)
                         )
                         (Span (SourceId 0) 16 17)
                       )
                       (Span (SourceId 0) 4 17)
                     , Binding
                       (Just (Ident "map" (Span (SourceId 0) 23 26)))
                       [ BindP (Ident "f" (Span (SourceId 0) 29 30))
                               (Span (SourceId 0) 29 30)
                       , CustomP
                         (BindP (Ident "x" (Span (SourceId 0) 32 33))
                                (Span (SourceId 0) 32 33)
                         )
                         (Operator "~" (Span (SourceId 0) 33 34))
                         (BindP (Ident "xs" (Span (SourceId 0) 34 36))
                                (Span (SourceId 0) 34 36)
                         )
                         (Span (SourceId 0) 31 37)
                       ]
                       (BinOpE
                         (GroupedE
                           (BinOpE
                             (VariableE
                               (Namespace
                                 [Ident "f" (Span (SourceId 0) 41 42)]
                                 (Span (SourceId 0) 41 42)
                               )
                               (Span (SourceId 0) 41 42)
                             )
                             (Operator "application" (Span (SourceId 0) 42 43))
                             (VariableE
                               (Namespace
                                 [Ident "x" (Span (SourceId 0) 43 44)]
                                 (Span (SourceId 0) 43 44)
                               )
                               (Span (SourceId 0) 43 44)
                             )
                             (Span (SourceId 0) 41 44)
                           )
                           (Span (SourceId 0) 40 45)
                         )
                         (Operator "~" (Span (SourceId 0) 46 47))
                         (GroupedE
                           (BinOpE
                             (BinOpE
                               (VariableE
                                 (Namespace
                                   [Ident "map" (Span (SourceId 0) 49 52)]
                                   (Span (SourceId 0) 49 52)
                                 )
                                 (Span (SourceId 0) 49 52)
                               )
                               (Operator "application" (Span (SourceId 0) 52 53)
                               )
                               (VariableE
                                 (Namespace
                                   [Ident "f" (Span (SourceId 0) 53 54)]
                                   (Span (SourceId 0) 53 54)
                                 )
                                 (Span (SourceId 0) 53 54)
                               )
                               (Span (SourceId 0) 49 54)
                             )
                             (Operator "application" (Span (SourceId 0) 54 55))
                             (VariableE
                               (Namespace
                                 [Ident "xs" (Span (SourceId 0) 55 57)]
                                 (Span (SourceId 0) 55 57)
                               )
                               (Span (SourceId 0) 55 57)
                             )
                             (Span (SourceId 0) 49 57)
                           )
                           (Span (SourceId 0) 48 58)
                         )
                         (Span (SourceId 0) 40 58)
                       )
                       (Span (SourceId 0) 23 58)
                     ]
                     (Span (SourceId 0) 0 58)
                   )
