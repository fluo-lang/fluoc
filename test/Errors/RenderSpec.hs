module Errors.RenderSpec where

import           Test.Hspec                     ( it
                                                , shouldBe
                                                , Spec
                                                , describe
                                                )
import qualified Data.Map                      as D

import           Errors.Render
import           Sources

sid :: SourceId
sid = SourceId 0

withFile :: String -> RenderD a -> (a, String)
withFile s r =
  runRender r (RS defaultConfig (D.singleton sid s) (D.singleton sid "test.fl"))

spec :: Spec
spec = do
  describe "Errors.Render.getLineCol" $ do
    it "should return the proper line and col nums"
      $          withFile "abcde\nabd\n10asd" (getLineCol sid 10)
      `shouldBe` ((2, 0), "")
  describe "Errors.Render.getFilename" $ do
    it "should get the filename"
      $          withFile "dummy contents" (getFilename sid)
      `shouldBe` ("test.fl", "")
  describe "Errors.Render.getFile" $ do
    it "should get the file contents"
      $          withFile "value test test" (getFile sid)
      `shouldBe` ("value test test", "")
  describe "Errors.Render.renderHeader" $ do
    it "set a file header"
      $          withFile "12345\n1230" (renderHeader "error" 1 "dummy message")
      `shouldBe` ((), "error[E001]: dummy message\n")
  describe "Errors.Render.renderSnippetState" $ do
    it "set a file state"
      $          withFile "asdbc\n12ads\nasddawd" (renderSnippetState sid 9)
      `shouldBe` ((), "┌─ test.fl:2:4\n")
