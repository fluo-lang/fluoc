module TestUtil where

import           Test.Hspec                     ( Expectation
                                                , shouldBe
                                                )
import           Syntax.ParserGeneric
import           Syntax.Token
import           Display

testParser
  :: (Show a, Show b, Eq a, Eq b, Display b)
  => [b]
  -> Parser b a
  -> ParserReturn b a
  -> Expectation
testParser source (P fn) x = realRes `shouldBe` expectedRes
 where
  realRes     = fn source dummySpanLimited
  expectedRes = x
