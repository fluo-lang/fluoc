module Syntax.ParserSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

spec :: Spec
spec = do
  describe "Syntax.Parser.parse" $ do
    it "should be empty on empty input" $ do
    
