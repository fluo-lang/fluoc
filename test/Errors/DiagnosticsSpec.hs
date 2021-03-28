module Errors.DiagnosticsSpec where

import           Test.Hspec                     ( it
                                                , shouldBe
                                                , Spec
                                                , describe
                                                )
import           Errors.Diagnostics

spec :: Spec
spec = do
  describe "Errors.Diagnostics.getLineColStr" $ do
    it "should return the proper line and col nums"
      $          getLineColStr "123\n123451\n12345" 11 (0, 0)
      `shouldBe` (2, 0)
    it "should return the proper line and col nums"
      $          getLineColStr "abcde\nabd\n10asd" 10 (0, 0)
      `shouldBe` (2, 0)
