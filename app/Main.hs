module Main where

-- import           Pipeline                       ( pipeline
--                                                 , runCompiler
--                                                 )

import qualified Data.Map                      as D
import           Errors.Render
import           Errors.Diagnostics
import           Errors.Views
import           Sources
import qualified Data.Sequence                 as S

main :: IO ()
main = do
  putStr $ snd $ runRender
    (renderDiagnostic $ Diagnostic
      Error
      SyntaxError
      [ Annotation (Span (SourceId 0) 4 37)
                   (Just "This is a test diagnostic")
                   Error
      , Annotation (Span (SourceId 0) 1 2)
                   (Just "This is a test diagnostic")
                   Error
      ]
      (Span (SourceId 0) 4 37)
      Nothing
    )
    (RS
      defaultConfig
      (D.singleton (SourceId 0)
                   "This is \n a cool\ng\nf\ne\nd\nc\nb\na\n test line"
      )
      (D.singleton (SourceId 0) "test.fl")
      (D.singleton
        (SourceId 0)
        (S.fromList $ lines "This is \n a cool\ng\nf\ne\nd\nc\nb\na\n test line"
        )
      )
    )
