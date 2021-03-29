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
    (do
      renderSnippetSource
        2
        11
        "This is a cool test line"
        [SingleLabel Info 24 25 "Info! not bad, huh?"]
        2
        [(0, Error, TopMultiLabel 3), (1, Error, BottomLabel 3 "Multiline")]
      renderLineBreak 2 1 [(0, Error, LeftLabel)]
      renderEmptyLine 2 1 [(0, Error, LeftLabel)]
    )
    (RS defaultConfig
        (D.singleton (SourceId 0) "This is a cool test line")
        (D.singleton (SourceId 0) "test.fl")
    )
