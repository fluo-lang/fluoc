module Syntax.ParserSpec where

import           Test.Hspec                     ( describe
                                                , it
                                                , shouldBe
                                                , Spec
                                                )

import           Sources
import           Syntax.Ast
import           Syntax.Parser

sid :: SourceId
sid = SourceId 0
sn :: Int -> Int -> Span
sn = Span sid

spec :: Spec
spec = do
  describe "Syntax.Parser.parseBlock" $ do
    it "should return empty list on empty input"
      $          parseBlock sid ""
      `shouldBe` Right []
    it "should parse a opdef statement"
      $          parseBlock
                   sid
                   "opdef (+) left binary 6\
\opdef (-) right prefix 3\
\opdef (--) left postfix 2\
\opdef (/) left binary 5"
      `shouldBe` Right
                   [ OpDefS (OpInfo (Operator "+" (sn 7 8)) LeftA Binary 6)
                            (sn 0 23)
                   , OpDefS (OpInfo (Operator "-" (sn 30 31)) RightA Prefix 3)
                            (sn 23 47)
                   , OpDefS
                     (OpInfo (Operator "--" (sn 54 56)) LeftA Postfix 2)
                     (sn 47 72)
                   , OpDefS (OpInfo (Operator "/" (sn 79 80)) LeftA Binary 5)
                            (sn 72 95)
                   ]
    it "should parse a record with multiple sums"
      $          parseBlock
                   sid
                   "rec Point = Point2D Int Int | Point3D Int Int Int | PointN { dec dims : Int dec values : List Int } "
      `shouldBe` Right
                   [ RecordS
                       (Ident "Point" (sn 4 9))
                       []
                       [ Product
                         (Ident "Point2D" (sn 12 19))
                         [ NamespaceType
                           (Namespace [Ident "Int" (sn 20 23)] (sn 20 23))
                           (sn 20 23)
                         , NamespaceType
                           (Namespace [Ident "Int" (sn 24 27)] (sn 24 27))
                           (sn 24 27)
                         ]
                         (sn 12 27)
                       , Product
                         (Ident "Point3D" (sn 30 37))
                         [ NamespaceType
                           (Namespace [Ident "Int" (sn 38 41)] (sn 38 41))
                           (sn 38 41)
                         , NamespaceType
                           (Namespace [Ident "Int" (sn 42 45)] (sn 42 45))
                           (sn 42 45)
                         , NamespaceType
                           (Namespace [Ident "Int" (sn 46 49)] (sn 46 49))
                           (sn 46 49)
                         ]
                         (sn 30 49)
                       , NamedProduct
                         (Ident "PointN" (sn 52 58))
                         [ Declaration
                           (Ident "dims" (sn 65 69))
                           (NamespaceType
                             (Namespace [Ident "Int" (sn 72 75)] (sn 72 75))
                             (sn 72 75)
                           )
                           (sn 61 75)
                         , Declaration
                           (Ident "values" (sn 80 86))
                           (OperatorType
                             (BinOp
                               (Operator "application" (sn 93 94))
                               (NamespaceType
                                 (Namespace [Ident "List" (sn 89 93)] (sn 89 93)
                                 )
                                 (sn 89 93)
                               )
                               (NamespaceType
                                 (Namespace [Ident "Int" (sn 94 97)] (sn 94 97))
                                 (sn 94 97)
                               )
                             )
                             (sn 89 97)
                           )
                           (sn 76 97)
                         ]
                         (sn 52 99)
                       ]
                       (sn 0 99)
                   ]
    it "should parse a record with multiple fields"
      $          parseBlock sid "rec Point = Point Int Bool Int"
      `shouldBe` Right
                   [ RecordS
                       (Ident "Point" (sn 4 9))
                       []
                       [ Product
                           (Ident "Point" (sn 12 17))
                           [ NamespaceType
                             (Namespace [Ident "Int" (sn 18 21)] (sn 18 21))
                             (sn 18 21)
                           , NamespaceType
                             (Namespace [Ident "Bool" (sn 22 26)] (sn 22 26))
                             (sn 22 26)
                           , NamespaceType
                             (Namespace [Ident "Int" (sn 27 30)] (sn 27 30))
                             (sn 27 30)
                           ]
                           (sn 12 30)
                       ]
                       (sn 0 30)
                   ]
    it "should parse a record with function"
      $          parseBlock sid "rec Point = Point (Int -> Int) Int"
      `shouldBe` Right
                   [ RecordS
                       (Ident "Point" (sn 4 9))
                       []
                       [ Product
                           (Ident "Point" (sn 12 17))
                           [ OperatorType
                             (Grouped
                               (OperatorType
                                 (BinOp
                                   (Operator "->" (sn 23 25))
                                   (NamespaceType
                                     (Namespace [Ident "Int" (sn 19 22)]
                                                (sn 19 22)
                                     )
                                     (sn 19 22)
                                   )
                                   (NamespaceType
                                     (Namespace [Ident "Int" (sn 26 29)]
                                                (sn 26 29)
                                     )
                                     (sn 26 29)
                                   )
                                 )
                                 (sn 19 29)
                               )
                             )
                             (sn 18 30)
                           , NamespaceType
                             (Namespace [Ident "Int" (sn 31 34)] (sn 31 34))
                             (sn 31 34)
                           ]
                           (sn 12 34)
                       ]
                       (sn 0 34)
                   ]
    it "should parse multiple lets"
      $          parseBlock sid "let a = 10, b = 20 let c = 10"
      `shouldBe` Right
                   [ BindingS
                     [ Binding Nothing
                               [BindP (Ident "a" (sn 4 5)) (sn 4 5)]
                               (LiteralE (IntegerL 10 (sn 8 10)) (sn 8 10))
                               (sn 4 10)
                     , Binding Nothing
                               [BindP (Ident "b" (sn 12 13)) (sn 12 13)]
                               (LiteralE (IntegerL 20 (sn 16 18)) (sn 16 18))
                               (sn 12 18)
                     ]
                     (sn 0 18)
                   , BindingS
                     [ Binding Nothing
                               [BindP (Ident "c" (sn 23 24)) (sn 23 24)]
                               (LiteralE (IntegerL 10 (sn 27 29)) (sn 27 29))
                               (sn 23 29)
                     ]
                     (sn 19 29)
                   ]
    it "should parse a unary operator"
      $          parseBlock sid "dec (?) : Int -> Int"
      `shouldBe` Right
                   [ DeclarationS
                       (Declaration
                         (OpId (Operator "?" (sn 5 6)))
                         (OperatorType
                           (BinOp
                             (Operator "->" (sn 14 16))
                             (NamespaceType
                               (Namespace [Ident "Int" (sn 10 13)] (sn 10 13))
                               (sn 10 13)
                             )
                             (NamespaceType
                               (Namespace [Ident "Int" (sn 17 20)] (sn 17 20))
                               (sn 17 20)
                             )
                           )
                           (sn 10 20)
                         )
                         (sn 0 20)
                       )
                       (sn 0 20)
                   ]
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
    it "should parse a let with multiple clauses"
      $          parseBlock
                   sid
                   "let map : f a = a,\n\
                   \    map : f (x~xs) = (f x) ~ (map f xs)"
      `shouldBe` Right
                   [ BindingS
                       [ Binding
                         (Just (Ident "map" (sn 4 7)))
                         [ BindP (Ident "f" (sn 10 11)) (sn 10 11)
                         , BindP (Ident "a" (sn 12 13)) (sn 12 13)
                         ]
                         (VariableE
                           (Namespace [Ident "a" (sn 16 17)] (sn 16 17))
                           (sn 16 17)
                         )
                         (sn 4 17)
                       , Binding
                         (Just (Ident "map" (sn 23 26)))
                         [ BindP (Ident "f" (sn 29 30)) (sn 29 30)
                         , OperatorP
                           (Grouped
                             (OperatorP
                               (BinOp
                                 (Operator "~" (sn 33 34))
                                 (BindP (Ident "x" (sn 32 33)) (sn 32 33))
                                 (BindP (Ident "xs" (sn 34 36)) (sn 34 36))
                               )
                               (sn 32 36)
                             )
                           )
                           (sn 31 37)
                         ]
                         (OperatorE
                           (BinOp
                             (Operator "~" (sn 46 47))
                             (GroupedE
                               (OperatorE
                                 (BinOp
                                   (Operator "application" (sn 42 43))
                                   (VariableE
                                     (Namespace [Ident "f" (sn 41 42)]
                                                (sn 41 42)
                                     )
                                     (sn 41 42)
                                   )
                                   (VariableE
                                     (Namespace [Ident "x" (sn 43 44)]
                                                (sn 43 44)
                                     )
                                     (sn 43 44)
                                   )
                                 )
                                 (sn 41 44)
                               )
                               (sn 40 45)
                             )
                             (GroupedE
                               (OperatorE
                                 (BinOp
                                   (Operator "application" (sn 54 55))
                                   (OperatorE
                                     (BinOp
                                       (Operator "application" (sn 52 53))
                                       (VariableE
                                         (Namespace [Ident "map" (sn 49 52)]
                                                    (sn 49 52)
                                         )
                                         (sn 49 52)
                                       )
                                       (VariableE
                                         (Namespace [Ident "f" (sn 53 54)]
                                                    (sn 53 54)
                                         )
                                         (sn 53 54)
                                       )
                                     )
                                     (sn 49 54)
                                   )
                                   (VariableE
                                     (Namespace [Ident "xs" (sn 55 57)]
                                                (sn 55 57)
                                     )
                                     (sn 55 57)
                                   )
                                 )
                                 (sn 49 57)
                               )
                               (sn 48 58)
                             )
                           )
                           (sn 40 58)
                         )
                         (sn 23 58)
                       ]
                       (sn 0 58)
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
                   (OperatorType
                     (BinOp
                       (Operator "->" (sn 7 9))
                       (NamespaceType
                         (Namespace [Ident "String" (sn 0 6)] (sn 0 6))
                         (sn 0 6)
                       )
                       (OperatorType
                         (BinOp
                           (Operator "->" (sn 14 16))
                           (NamespaceType
                             (Namespace [Ident "Int" (sn 10 13)] (sn 10 13))
                             (sn 10 13)
                           )
                           (NamespaceType
                             (Namespace [Ident "Bool" (sn 17 21)] (sn 17 21))
                             (sn 17 21)
                           )
                         )
                         (sn 10 21)
                       )
                     )
                     (sn 0 21)
                   )
    it "should parse a type application"
      $          parseType sid "Option Int Float"
      `shouldBe` Right
                   (OperatorType
                     (BinOp
                       (Operator "application" (sn 10 11))
                       (OperatorType
                         (BinOp
                           (Operator "application" (sn 6 7))
                           (NamespaceType
                             (Namespace [Ident "Option" (sn 0 6)] (sn 0 6))
                             (sn 0 6)
                           )
                           (NamespaceType
                             (Namespace [Ident "Int" (sn 7 10)] (sn 7 10))
                             (sn 7 10)
                           )
                         )
                         (sn 0 10)
                       )
                       (NamespaceType
                         (Namespace [Ident "Float" (sn 11 16)] (sn 11 16))
                         (sn 11 16)
                       )
                     )
                     (sn 0 16)
                   )
    it "should parse a type application with parens"
      $          parseType sid "Option (Int Float)"
      `shouldBe` Right
                   (OperatorType
                     (BinOp
                       (Operator "application" (sn 6 7))
                       (NamespaceType
                         (Namespace [Ident "Option" (sn 0 6)] (sn 0 6))
                         (sn 0 6)
                       )
                       (OperatorType
                         (Grouped
                           (OperatorType
                             (BinOp
                               (Operator "application" (sn 11 12))
                               (NamespaceType
                                 (Namespace [Ident "Int" (sn 8 11)] (sn 8 11))
                                 (sn 8 11)
                               )
                               (NamespaceType
                                 (Namespace [Ident "Float" (sn 12 17)]
                                            (sn 12 17)
                                 )
                                 (sn 12 17)
                               )
                             )
                             (sn 8 17)
                           )
                         )
                         (sn 7 18)
                       )
                     )
                     (sn 0 18)
                   )
    it "should parse a function type with applications"
      $          parseType sid "Option Int -> Int"
      `shouldBe` Right
                   (OperatorType
                     (BinOp
                       (Operator "->" (sn 11 13))
                       (OperatorType
                         (BinOp
                           (Operator "application" (sn 6 7))
                           (NamespaceType
                             (Namespace [Ident "Option" (sn 0 6)] (sn 0 6))
                             (sn 0 6)
                           )
                           (NamespaceType
                             (Namespace [Ident "Int" (sn 7 10)] (sn 7 10))
                             (sn 7 10)
                           )
                         )
                         (sn 0 10)
                       )
                       (NamespaceType
                         (Namespace [Ident "Int" (sn 14 17)] (sn 14 17))
                         (sn 14 17)
                       )
                     )
                     (sn 0 17)
                   )
    it "should parse a function type with parens"
      $          parseType sid "Option (Int -> Int)"
      `shouldBe` Right
                   (OperatorType
                     (BinOp
                       (Operator "application" (sn 6 7))
                       (NamespaceType
                         (Namespace [Ident "Option" (sn 0 6)] (sn 0 6))
                         (sn 0 6)
                       )
                       (OperatorType
                         (Grouped
                           (OperatorType
                             (BinOp
                               (Operator "->" (sn 12 14))
                               (NamespaceType
                                 (Namespace [Ident "Int" (sn 8 11)] (sn 8 11))
                                 (sn 8 11)
                               )
                               (NamespaceType
                                 (Namespace [Ident "Int" (sn 15 18)] (sn 15 18))
                                 (sn 15 18)
                               )
                             )
                             (sn 8 18)
                           )
                         )
                         (sn 7 19)
                       )
                     )
                     (sn 0 19)
                   )
    it "should parse a more complex type"
      $ parseType sid "Option (Int -> Int) -> (Option Int -> Int) -> Int -> Int"
      `shouldBe` Right
                   (OperatorType
                     (BinOp
                       (Operator "->" (sn 20 22))
                       (OperatorType
                         (BinOp
                           (Operator "application" (sn 6 7))
                           (NamespaceType
                             (Namespace [Ident "Option" (sn 0 6)] (sn 0 6))
                             (sn 0 6)
                           )
                           (OperatorType
                             (Grouped
                               (OperatorType
                                 (BinOp
                                   (Operator "->" (sn 12 14))
                                   (NamespaceType
                                     (Namespace [Ident "Int" (sn 8 11)]
                                                (sn 8 11)
                                     )
                                     (sn 8 11)
                                   )
                                   (NamespaceType
                                     (Namespace [Ident "Int" (sn 15 18)]
                                                (sn 15 18)
                                     )
                                     (sn 15 18)
                                   )
                                 )
                                 (sn 8 18)
                               )
                             )
                             (sn 7 19)
                           )
                         )
                         (sn 0 19)
                       )
                       (OperatorType
                         (BinOp
                           (Operator "->" (sn 43 45))
                           (OperatorType
                             (Grouped
                               (OperatorType
                                 (BinOp
                                   (Operator "->" (sn 35 37))
                                   (OperatorType
                                     (BinOp
                                       (Operator "application" (sn 30 31))
                                       (NamespaceType
                                         (Namespace
                                           [Ident "Option" (sn 24 30)]
                                           (sn 24 30)
                                         )
                                         (sn 24 30)
                                       )
                                       (NamespaceType
                                         (Namespace [Ident "Int" (sn 31 34)]
                                                    (sn 31 34)
                                         )
                                         (sn 31 34)
                                       )
                                     )
                                     (sn 24 34)
                                   )
                                   (NamespaceType
                                     (Namespace [Ident "Int" (sn 38 41)]
                                                (sn 38 41)
                                     )
                                     (sn 38 41)
                                   )
                                 )
                                 (sn 24 41)
                               )
                             )
                             (sn 23 42)
                           )
                           (OperatorType
                             (BinOp
                               (Operator "->" (sn 50 52))
                               (NamespaceType
                                 (Namespace [Ident "Int" (sn 46 49)] (sn 46 49))
                                 (sn 46 49)
                               )
                               (NamespaceType
                                 (Namespace [Ident "Int" (sn 53 56)] (sn 53 56))
                                 (sn 53 56)
                               )
                             )
                             (sn 46 56)
                           )
                         )
                         (sn 23 56)
                       )
                     )
                     (sn 0 56)
                   )
  describe "Syntax.Parser.parseExpr" $ do
    it "should parse a literal" $ parseExpr sid "10" `shouldBe` Right
      (LiteralE (IntegerL 10 (sn 0 2)) (sn 0 2))
    it "should parse a function application"
      $          parseExpr sid "1 2"
      `shouldBe` Right
                   (OperatorE
                     (BinOp (Operator "application" (sn 1 2))
                            (LiteralE (IntegerL 1 (sn 0 1)) (sn 0 1))
                            (LiteralE (IntegerL 2 (sn 2 3)) (sn 2 3))
                     )
                     (sn 0 3)
                   )
    it "should parse a function application with multiple"
      $          parseExpr sid "1 2 3"
      `shouldBe` Right
                   (OperatorE
                     (BinOp
                       (Operator "application" (sn 3 4))
                       (OperatorE
                         (BinOp (Operator "application" (sn 1 2))
                                (LiteralE (IntegerL 1 (sn 0 1)) (sn 0 1))
                                (LiteralE (IntegerL 2 (sn 2 3)) (sn 2 3))
                         )
                         (sn 0 3)
                       )
                       (LiteralE (IntegerL 3 (sn 4 5)) (sn 4 5))
                     )
                     (sn 0 5)
                   )
    it "should parse a function application with multiple and grouping"
      $          parseExpr sid "1 (2 3)"
      `shouldBe` Right
                   (OperatorE
                     (BinOp
                       (Operator "application" (sn 1 2))
                       (LiteralE (IntegerL 1 (sn 0 1)) (sn 0 1))
                       (GroupedE
                         (OperatorE
                           (BinOp (Operator "application" (sn 4 5))
                                  (LiteralE (IntegerL 2 (sn 3 4)) (sn 3 4))
                                  (LiteralE (IntegerL 3 (sn 5 6)) (sn 5 6))
                           )
                           (sn 3 6)
                         )
                         (sn 2 7)
                       )
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
      (OperatorE
        (BinOp (Operator "+" (sn 3 4))
               (LiteralE (IntegerL 10 (sn 0 2)) (sn 0 2))
               (LiteralE (IntegerL 20 (sn 5 7)) (sn 5 7))
        )
        (sn 0 7)
      )
    it "should parse infix and postfix operators"
      $          parseExpr sid "(+10) (5!)"
      `shouldBe` Right
                   (OperatorE
                     (BinOp
                       (Operator "application" (sn 5 6))
                       (OperatorE
                         (PreOp (Operator "+" (sn 1 2))
                                (LiteralE (IntegerL 10 (sn 2 4)) (sn 2 4))
                         )
                         (sn 0 5)
                       )
                       (OperatorE
                         (PostOp (Operator "!" (sn 8 9))
                                 (LiteralE (IntegerL 5 (sn 7 8)) (sn 7 8))
                         )
                         (sn 6 10)
                       )
                     )
                     (sn 0 10)
                   )
    it "should parse multiple operators"
      $          parseExpr sid "10 + 20 ++ 30"
      `shouldBe` Right
                   (OperatorE
                     (BinOp
                       (Operator "++" (sn 8 10))
                       (OperatorE
                         (BinOp (Operator "+" (sn 3 4))
                                (LiteralE (IntegerL 10 (sn 0 2)) (sn 0 2))
                                (LiteralE (IntegerL 20 (sn 5 7)) (sn 5 7))
                         )
                         (sn 0 7)
                       )
                       (LiteralE (IntegerL 30 (sn 11 13)) (sn 11 13))
                     )
                     (sn 0 13)
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
    it "should parse a basic assign in binding"
      $          parseExpr sid "assign x = 10 in { 30 }"
      `shouldBe` Right
                   (LetInE
                     [ Binding Nothing
                               [BindP (Ident "x" (sn 7 8)) (sn 7 8)]
                               (LiteralE (IntegerL 10 (sn 11 13)) (sn 11 13))
                               (sn 7 13)
                     ]
                     (LiteralE (IntegerL 30 (sn 19 21)) (sn 19 21))
                     (sn 0 23)
                   )
    it "should parse a pattern destructure"
      $          parseExpr sid "assign x y = 10 in { 30 40 }"
      `shouldBe` Right
                   (LetInE
                     [ Binding
                         Nothing
                         [ OperatorP
                             (BinOp
                               (Operator "application" (sn 8 9))
                               (NamespaceP
                                 (Namespace [Ident "x" (sn 7 8)] (sn 7 8))
                                 (sn 7 8)
                               )
                               (BindP (Ident "y" (sn 9 10)) (sn 9 10))
                             )
                             (sn 7 10)
                         ]
                         (LiteralE (IntegerL 10 (sn 13 15)) (sn 13 15))
                         (sn 7 15)
                     ]
                     (OperatorE
                       (BinOp (Operator "application" (sn 23 24))
                              (LiteralE (IntegerL 30 (sn 21 23)) (sn 21 23))
                              (LiteralE (IntegerL 40 (sn 24 26)) (sn 24 26))
                       )
                       (sn 21 26)
                     )
                     (sn 0 28)
                   )
    it "should parse a single pattern with parens"
      $          parseExpr sid "assign (x y) = 10 in { 30 }"
      `shouldBe` Right
                   (LetInE
                     [ Binding
                         Nothing
                         [ OperatorP
                             (Grouped
                               (OperatorP
                                 (BinOp
                                   (Operator "application" (sn 9 10))
                                   (NamespaceP
                                     (Namespace [Ident "x" (sn 8 9)] (sn 8 9))
                                     (sn 8 9)
                                   )
                                   (BindP (Ident "y" (sn 10 11)) (sn 10 11))
                                 )
                                 (sn 8 11)
                               )
                             )
                             (sn 7 12)
                         ]
                         (LiteralE (IntegerL 10 (sn 15 17)) (sn 15 17))
                         (sn 7 17)
                     ]
                     (LiteralE (IntegerL 30 (sn 23 25)) (sn 23 25))
                     (sn 0 27)
                   )
    it "should parse multiple values with operator"
      $          parseExpr sid "assign (Some x~xs) = 10 in { 30 }"
      `shouldBe` Right
                   (LetInE
                     [ Binding
                         Nothing
                         [ OperatorP
                             (Grouped
                               (OperatorP
                                 (BinOp
                                   (Operator "application" (sn 12 13))
                                   (NamespaceP
                                     (Namespace [Ident "Some" (sn 8 12)]
                                                (sn 8 12)
                                     )
                                     (sn 8 12)
                                   )
                                   (OperatorP
                                     (BinOp
                                       (Operator "~" (sn 14 15))
                                       (BindP (Ident "x" (sn 13 14)) (sn 13 14))
                                       (BindP (Ident "xs" (sn 15 17)) (sn 15 17)
                                       )
                                     )
                                     (sn 13 17)
                                   )
                                 )
                                 (sn 8 17)
                               )
                             )
                             (sn 7 18)
                         ]
                         (LiteralE (IntegerL 10 (sn 21 23)) (sn 21 23))
                         (sn 7 23)
                     ]
                     (LiteralE (IntegerL 30 (sn 29 31)) (sn 29 31))
                     (sn 0 33)
                   )
    it "should parse a function"
      $          parseExpr sid "assign myfn : (Some x~xs) a = x+a in { 30 }"
      `shouldBe` Right
                   (LetInE
                     [ Binding
                         (Just (Ident "myfn" (sn 7 11)))
                         [ OperatorP
                           (Grouped
                             (OperatorP
                               (BinOp
                                 (Operator "application" (sn 19 20))
                                 (NamespaceP
                                   (Namespace [Ident "Some" (sn 15 19)]
                                              (sn 15 19)
                                   )
                                   (sn 15 19)
                                 )
                                 (OperatorP
                                   (BinOp
                                     (Operator "~" (sn 21 22))
                                     (BindP (Ident "x" (sn 20 21)) (sn 20 21))
                                     (BindP (Ident "xs" (sn 22 24)) (sn 22 24))
                                   )
                                   (sn 20 24)
                                 )
                               )
                               (sn 15 24)
                             )
                           )
                           (sn 14 25)
                         , BindP (Ident "a" (sn 26 27)) (sn 26 27)
                         ]
                         (OperatorE
                           (BinOp
                             (Operator "+" (sn 31 32))
                             (VariableE
                               (Namespace [Ident "x" (sn 30 31)] (sn 30 31))
                               (sn 30 31)
                             )
                             (VariableE
                               (Namespace [Ident "a" (sn 32 33)] (sn 32 33))
                               (sn 32 33)
                             )
                           )
                           (sn 30 33)
                         )
                         (sn 7 33)
                     ]
                     (LiteralE (IntegerL 30 (sn 39 41)) (sn 39 41))
                     (sn 0 43)
                   )
    it "should parse match statement"
      $          parseExpr sid "match 10 { 10 => x }"
      `shouldBe` Right
                   (MatchE
                     (LiteralE (IntegerL 10 (sn 6 8)) (sn 6 8))
                     [ MatchBranch
                         (LiteralP (IntegerL 10 (sn 11 13)) (sn 11 13))
                         (VariableE
                           (Namespace [Ident "x" (sn 17 18)] (sn 17 18))
                           (sn 17 18)
                         )
                         (sn 11 18)
                     ]
                     (sn 0 20)
                   )
    it "should parse multiple clauses in match statement"
      $          parseExpr sid "match 10 { 10 => x, 20 => y, _ => 30 }"
      `shouldBe` Right
                   (MatchE
                     (LiteralE (IntegerL 10 (sn 6 8)) (sn 6 8))
                     [ MatchBranch
                       (LiteralP (IntegerL 10 (sn 11 13)) (sn 11 13))
                       (VariableE
                         (Namespace [Ident "x" (sn 17 18)] (sn 17 18))
                         (sn 17 18)
                       )
                       (sn 11 18)
                     , MatchBranch
                       (LiteralP (IntegerL 20 (sn 20 22)) (sn 20 22))
                       (VariableE
                         (Namespace [Ident "y" (sn 26 27)] (sn 26 27))
                         (sn 26 27)
                       )
                       (sn 20 27)
                     , MatchBranch
                       (DropP (sn 29 30))
                       (LiteralE (IntegerL 30 (sn 34 36)) (sn 34 36))
                       (sn 29 36)
                     ]
                     (sn 0 38)
                   )
    it "should parse lambda expressions"
      $          parseExpr sid "\\a b => a + b"
      `shouldBe` Right
                   (LambdaE
                     [ BindP (Ident "a" (sn 1 2)) (sn 1 2)
                     , BindP (Ident "b" (sn 3 4)) (sn 3 4)
                     ]
                     (OperatorE
                       (BinOp
                         (Operator "+" (sn 10 11))
                         (VariableE (Namespace [Ident "a" (sn 8 9)] (sn 8 9))
                                    (sn 8 9)
                         )
                         (VariableE
                           (Namespace [Ident "b" (sn 12 13)] (sn 12 13))
                           (sn 12 13)
                         )
                       )
                       (sn 8 13)
                     )
                     (sn 0 13)
                   )
    it "should parse nested lambda expressions"
      $          parseExpr sid "\\a b c => \\d => a + b + c + d"
      `shouldBe` Right
                   (LambdaE
                     [ BindP (Ident "a" (sn 1 2)) (sn 1 2)
                     , BindP (Ident "b" (sn 3 4)) (sn 3 4)
                     , BindP (Ident "c" (sn 5 6)) (sn 5 6)
                     ]
                     (LambdaE
                       [BindP (Ident "d" (sn 11 12)) (sn 11 12)]
                       (OperatorE
                         (BinOp
                           (Operator "+" (sn 26 27))
                           (OperatorE
                             (BinOp
                               (Operator "+" (sn 22 23))
                               (OperatorE
                                 (BinOp
                                   (Operator "+" (sn 18 19))
                                   (VariableE
                                     (Namespace [Ident "a" (sn 16 17)]
                                                (sn 16 17)
                                     )
                                     (sn 16 17)
                                   )
                                   (VariableE
                                     (Namespace [Ident "b" (sn 20 21)]
                                                (sn 20 21)
                                     )
                                     (sn 20 21)
                                   )
                                 )
                                 (sn 16 21)
                               )
                               (VariableE
                                 (Namespace [Ident "c" (sn 24 25)] (sn 24 25))
                                 (sn 24 25)
                               )
                             )
                             (sn 16 25)
                           )
                           (VariableE
                             (Namespace [Ident "d" (sn 28 29)] (sn 28 29))
                             (sn 28 29)
                           )
                         )
                         (sn 16 29)
                       )
                       (sn 10 29)
                     )
                     (sn 0 29)
                   )
