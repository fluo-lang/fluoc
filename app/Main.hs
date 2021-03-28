module Main where

-- import           Pipeline                       ( pipeline
--                                                 , runCompiler
--                                                 )

import qualified Data.Map                      as D
import           Errors.Render
import           Errors.Diagnostics
import           Sources

main :: IO ()
main = do
  putStr $ snd $ runRender
    (renderSnippetSource 2
                         11
                         "This is a cool test line"
                         [SingleLabel Info 11 20 "Info! not bad, huh?"]
                         0
                         [(1, Error, BottomLabel 3 "Multiline")]
    )
    (RS defaultConfig
        (D.singleton (SourceId 0) "This is a cool test line")
        (D.singleton (SourceId 0) "test.fl")
    )
