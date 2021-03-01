module Pipeline where

import           Options.Applicative

data Options = Options
  { filename :: String
  } deriving Show

options :: Parser Options
options = Options <$> argument str (metavar "<file>" <> help "The entry file with an `entry` function.")

pipeline :: IO ()
pipeline = do
  args <- execParser opts
  print $ args
 where
  opts = info
    (options <**> helper)
    (fullDesc <> progDesc "Compile a fluo program" <> header
      "fluoc - the official fluo compiler"
    )

