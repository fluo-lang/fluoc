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
      [ Annotation (Span (SourceId 0) 4 42)
                   (Just "This is a test diagnostic")
                   Error
      , Annotation (Span (SourceId 0) 27 28)
                   (Just "This is a test diagnostic")
                   Warning
      , Annotation (Span (SourceId 0) 1 2)
                   (Just "This is a test diagnostic")
                   Error
      , Annotation (Span (SourceId 1) 1 15)
                   (Just "This is a test diagnostic")
                   Info
      ]
      (Span (SourceId 0) 4 37)
      ["note: this is a dumb erro", "test a;slawkd;alwf"]
    )
    (RS
      defaultConfig
      (D.fromList
        [ ((SourceId 0), "This is \n a cool\ng\nf\ne\nd\nc\nb\na\n\na\na\na\na\natest line")
        , ((SourceId 1), "This is a cool\nline is multilne")
        ]
      )
      (D.fromList [((SourceId 0), "test.fl"), ((SourceId 1), "other_test.fl")])
      (D.fromList
        [ ( (SourceId 0)
          , ( S.fromList
            $ lines "This is \n a cool\ng\nf\ne\nd\nc\nb\na\n\na\na\na\na\natest line"
            )
          )
        , ( (SourceId 1)
          , ( S.fromList
            $ lines "This is a cool\nline is multilne"
            )
          )
        ]
      )
    )
