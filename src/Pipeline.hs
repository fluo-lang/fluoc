{-# LANGUAGE FlexibleContexts #-}
module Pipeline
  ( runCompiler
  , pipeline
  ) where

import           Data.Map                       ( insert )
import           Options.Applicative
import           System.IO
import           Control.Monad                  ( when )
import           Control.Monad.Except           ( ExceptT(..)
                                                , mapExceptT
                                                )
import           Control.Monad.State            ( liftIO
                                                , get
                                                , put
                                                , lift
                                                , StateT
                                                )
import           Control.Monad.Identity         ( runIdentity )
import           Control.Arrow                  ( left )
import           Syntax.PrettyPrint             ( PP(pp) )
import           Data.Tree                      ( drawTree )

import           Syntax.Ast                     ( Statement )
import           Syntax.Rewrite                 ( rewrite )
import           Syntax.Parser
import           Sources
import           Compiler                       ( Compiler(..)
                                                , CompilerState(..)
                                                , runCompiler
                                                , try
                                                )
import           Errors.Diagnostics             ( Diagnostics(..)
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

parseAndRewrite :: SourceId -> String -> Compiler [Statement]
parseAndRewrite sid source = do
  ast <- try $ parseBlock sid source
  try $ rewrite ast

pAst :: SourceId -> String -> Compiler ()
pAst sid source = do
  ast <- parseAndRewrite sid source
  liftIO . putStrLn . drawTree . pp $ ast

insertFile :: String -> String -> Compiler SourceId
insertFile f source = do
  old <- get
  let sid = (+ 1) `mapSid` currId old
  put $ CST (insert sid source $ sourceMap old) (insert sid f $ fileMap old) sid
  return sid

pipeline :: Compiler ()
pipeline = do
  args <- liftIO $ execParser opts
  let fname = filename args
  handle   <- liftIO $ openFile fname ReadMode
  contents <- liftIO $ readFile fname
  sid      <- insertFile fname contents
  when (printAst args) $ pAst sid contents
  res <- compileModule sid contents
  liftIO $ hClose handle
 where
  opts = info
    (options <**> helper)
    (  fullDesc
    <> progDesc "Compile a fluo program"
    <> header
         "fluoc - the official fluo compiler <https://github.com/fluo-lang/fluoc>"
    )

compileModule
  :: SourceId
  -> String
  -> Compiler [Statement]
compileModule = parseAndRewrite
