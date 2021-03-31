module Errors.RenderSpec where

import           Test.Hspec                     ( it
                                                , shouldBe
                                                , Spec
                                                , describe
                                                )
import qualified Data.Map                      as D
import           Data.List.Split                ( splitOn )
import qualified Data.Sequence                 as S

import           Errors.Render
import           Sources
import           Errors.Diagnostics

sid :: SourceId
sid = SourceId 0

withFile :: String -> RenderD a -> (a, String)
withFile s r = runRender
  r
  (RS defaultConfig
      (D.singleton sid s)
      (D.singleton sid "test.fl")
      (D.singleton sid (S.fromList $ splitOn "\n" s))
  )

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
      $ withFile "12345\n1230" (renderHeader Error "error" 1 "dummy message")
      `shouldBe` ( ()
                 , color defaultColorSet Error
                 ++ bold
                 ++ "error[E001]: dummy message\n"
                 ++ reset
                 )
  describe "Errors.Render.renderSnippetState" $ do
    it "set a file state"
      $          withFile "asdbc\n12ads\nasddawd" (renderSnippetState 0 sid 9)
      `shouldBe` ( ()
                 , " "
                 ++ gutterColor defaultColorSet
                 ++ "┌─"
                 ++ filenameColor defaultColorSet
                 ++ " test.fl:2:4\n"
                 ++ reset
                 )
