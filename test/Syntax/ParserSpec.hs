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
