module Pipeline
  ( runCompiler
  , pipeline
  ) where

import           Data.Maybe                     ( fromJust )
import           Data.Map                       ( insert )
import           Options.Applicative
import           System.IO
import           Control.Monad                  ( when )
import           Control.Monad.State            ( liftIO
                                                , get
                                                , put
                                                )
import           Control.Arrow                  ( left )
import           Text.Show.Pretty

import           Syntax.Ast                     ( Statement )
import           Syntax.Parser
import           Sources
import           Compiler
import           Errors.Diagnostics             ( report
                                                , Diagnostics(..)
                                                , intoDiagnostics
                                                )

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

pAst :: SourceId -> String -> Compiler ()
pAst sid source = case parseBlock sid source of
  Left e -> report $ intoDiagnostics e
  Right ast ->
    liftIO $ putStrLn $ valToStr $ hideCon True (== "Span") $ fromJust $ reify
      ast

insertFile :: String -> String -> Compiler SourceId
insertFile f source = do
  old <- get
  let sid = (+ 1) `mapSid` currId old
  put $ CST (insert sid source $ sourceMap old) (insert sid f $ fileMap old) sid
  return sid

pipeline :: Compiler ()
pipeline = do
  args     <- liftIO $ execParser opts
  handle   <- liftIO $ openFile (filename args) ReadMode
  contents <- liftIO $ hGetContents handle
  sid      <- insertFile (filename args) contents
  when (printAst args) $ pAst sid contents
  let compiled = compileModule sid contents
  liftIO $ print compiled
  liftIO $ hClose handle
 where
  opts = info
    (options <**> helper)
    (  fullDesc
    <> progDesc "Compile a fluo program"
    <> header
         "fluoc - the official fluo compiler <https://github.com/fluo-lang/fluoc>"
    )

compileModule :: SourceId -> String -> Either Diagnostics [Statement]
compileModule sid source = do
  intoDiagnostics `left` parseBlock sid source
