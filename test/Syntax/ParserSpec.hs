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
    it "should parse a type application"
      $          parseType sid "Option Int Float"
      `shouldBe` Right
                   (TypeApplication
                     (TypeApplication
                       (NamespaceType
                         (Namespace [Ident "Option" $ sn 0 6] $ sn 0 6)
                         (sn 0 6)
                       )
                       (NamespaceType
                         (Namespace [Ident "Int" $ sn 7 10] $ sn 7 10)
                         (sn 7 10)
                       )
                       (sn 0 10)
                     )
                     (NamespaceType
                       (Namespace [Ident "Float" $ sn 11 16] $ sn 11 16)
                       (sn 11 16)
                     )
                     (sn 0 16)
                   )
    it "should parse a type application with parens"
      $          parseType sid "Option (Int Float)"
      `shouldBe` Right
                   (TypeApplication
                     (NamespaceType
                       (Namespace [Ident "Option" $ sn 0 6] $ sn 0 6)
                       (sn 0 6)
                     )
                     (TypeApplication
                       (NamespaceType
                         (Namespace [Ident "Int" $ sn 8 11] $ sn 8 11)
                         (sn 8 11)
                       )
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
                   (FunctionType
                     (TypeApplication
                       (NamespaceType
                         (Namespace [Ident "Option" $ sn 0 6] $ sn 0 6)
                         (sn 0 6)
                       )
                       (NamespaceType
                         (Namespace [Ident "Int" $ sn 7 10] $ sn 7 10)
                         (sn 7 10)
                       )
                       (sn 0 10)
                     )
                     (NamespaceType
                       (Namespace [Ident "Int" $ sn 14 17] $ sn 14 17)
                       (sn 14 17)
                     )
                     (sn 0 17)
                   )
    it "should parse a function type with parens properly"
      $          parseType sid "Option (Int -> Int)"
      `shouldBe` Right
                   (TypeApplication
                     (NamespaceType
                       (Namespace [Ident "Option" $ sn 0 6] $ sn 0 6)
                       (sn 0 6)
                     )
                     (FunctionType
                       (NamespaceType
                         (Namespace [Ident "Int" $ sn 8 11] $ sn 8 11)
                         (sn 8 11)
                       )
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
                   (FunctionType
                     (FunctionType
                       (FunctionType
                         (TypeApplication
                           (NamespaceType
                             (Namespace
                               [Ident "Option" (Span (SourceId 0) 0 6)]
                               (Span (SourceId 0) 0 6)
                             )
                             (Span (SourceId 0) 0 6)
                           )
                           (FunctionType
                             (NamespaceType
                               (Namespace
                                 [Ident "Int" (Span (SourceId 0) 8 11)]
                                 (Span (SourceId 0) 8 11)
                               )
                               (Span (SourceId 0) 8 11)
                             )
                             (NamespaceType
                               (Namespace
                                 [Ident "Int" (Span (SourceId 0) 15 18)]
                                 (Span (SourceId 0) 15 18)
                               )
                               (Span (SourceId 0) 15 18)
                             )
                             (Span (SourceId 0) 7 19)
                           )
                           (Span (SourceId 0) 0 19)
                         )
                         (FunctionType
                           (TypeApplication
                             (NamespaceType
                               (Namespace
                                 [Ident "Option" (Span (SourceId 0) 24 30)]
                                 (Span (SourceId 0) 24 30)
                               )
                               (Span (SourceId 0) 24 30)
                             )
                             (NamespaceType
                               (Namespace
                                 [Ident "Int" (Span (SourceId 0) 31 34)]
                                 (Span (SourceId 0) 31 34)
                               )
                               (Span (SourceId 0) 31 34)
                             )
                             (Span (SourceId 0) 24 34)
                           )
                           (NamespaceType
                             (Namespace
                               [Ident "Int" (Span (SourceId 0) 38 41)]
                               (Span (SourceId 0) 38 41)
                             )
                             (Span (SourceId 0) 38 41)
                           )
                           (Span (SourceId 0) 23 42)
                         )
                         (Span (SourceId 0) 0 42)
                       )
                       (NamespaceType
                         (Namespace [Ident "Int" (Span (SourceId 0) 46 49)]
                                    (Span (SourceId 0) 46 49)
                         )
                         (Span (SourceId 0) 46 49)
                       )
                       (Span (SourceId 0) 0 49)
                     )
                     (NamespaceType
                       (Namespace [Ident "Int" (Span (SourceId 0) 53 56)]
                                  (Span (SourceId 0) 53 56)
                       )
                       (Span (SourceId 0) 53 56)
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
