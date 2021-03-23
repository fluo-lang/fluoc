module Pipeline where

import           Data.Maybe                     ( fromJust )
import           Options.Applicative
import           System.IO
import           Control.Monad                  ( when )
import           Text.Show.Pretty

import           Syntax.Parser
import           Sources

data Options = Options
  { filename :: String
  , output   :: String
  , verbose  :: Bool
  , printAst :: Bool
  }
  deriving Show

options :: Parser Options
options =
  Options
    <$> argument str (metavar "<path>" <> help "Specify the input file")
    <*> option
          auto
          (  long "output"
          <> short 'o'
          <> showDefault
          <> value "a.out"
          <> metavar "<path>"
          <> help "Set the output executable's path"
          )
    <*> switch
          (long "verbose" <> short 'v' <> help "Enable verbose compiler output")
    <*> switch
          (long "print-ast" <> help "Print the abstract syntax tree of the file"
          )

pAst :: SourceId -> String -> IO ()
pAst sid source = case parseBlock sid source of
  Left e -> print e
  Right ast ->
    putStrLn $ valToStr $ hideCon True (== "Span") $ fromJust $ reify ast

pipeline :: IO ()
pipeline = do
  args     <- execParser opts
  handle   <- openFile (filename args) ReadMode
  contents <- hGetContents handle
  when (printAst args) $ pAst (SourceId 0) contents
  hClose handle
 where
  opts = info
    (options <**> helper)
    (  fullDesc
    <> progDesc "Compile a fluo program"
    <> header
         "fluoc - the official fluo compiler <https://github.com/fluo-lang/fluoc>"
    )
