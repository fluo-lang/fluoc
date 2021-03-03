module Pipeline where

import           Options.Applicative

data Options = Options
  { filename :: String,
    output :: String,
    verbose :: Bool
  } deriving Show

options :: Parser Options
options =
  Options
    <$> argument
          str
          (metavar "<path>" <> help "Specify the input file")
    <*> option auto (long "output" <> short 'o' <> showDefault <> value "a.out" <> metavar "<path>" <> help "Set the output executable's path")
    <*> switch (long "verbose" <> short 'v' <> help "Enable verbose compiler output")

pipeline :: IO ()
pipeline = do
  args <- execParser opts
  print args
 where
  opts = info
    (options <**> helper)
    (  fullDesc
    <> progDesc "Compile a fluo program"
    <> header
         "fluoc - the official fluo compiler <https://github.com/fluo-lang/fluoc>"
    )
