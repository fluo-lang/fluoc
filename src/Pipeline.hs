{-# LANGUAGE FlexibleContexts #-}
module Pipeline
  ( runCompiler
  , pipeline
  ) where

import           Data.Map                       ( insert )
import           Options.Applicative
import           System.IO
import           Control.Monad                  ( when )
import           Control.Monad.State            ( liftIO
                                                , get
                                                , put
                                                )
import           Syntax.PrettyPrint             ( PP(pp) )
import           Data.Tree                      ( drawTree )
import           Syntax.Ast                     ( Statement )
import           Syntax.Rewrite                 ( rewrite )
import           DesugarBefore                  ( desugarBefore )
import           Syntax.Parser
import           Sources
import           Compiler                       ( Compiler(..)
                                                , CompilerState(..)
                                                , runCompiler
                                                , try
                                                )

data Options = Options
  { filename           :: String
  , output             :: String
  , verbose            :: Bool
  , printAst           :: Bool
  , printBeforeRewrite :: Bool
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
    <*> switch
          (  long "no-rewrite"
          <> help "Don't rewrite printed abstract syntax tree"
          )

pAst :: Bool -> SourceId -> String -> Compiler ()
pAst dontRewrite sid source = do
  ast <- try $ parseBlock sid source
  new <- if dontRewrite then return ast else try $ rewrite ast
  liftIO . print $ new
  liftIO . putStrLn . drawTree . pp $ new

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
  when (printAst args) $ pAst (printBeforeRewrite args) sid contents
  res <- compileModule sid contents
  liftIO $ print res
  liftIO $ hClose handle
 where
  opts = info
    (options <**> helper)
    (  fullDesc
    <> progDesc "Compile a fluo program"
    <> header
         "fluoc - the official fluo compiler <https://github.com/fluo-lang/fluoc>"
    )

compileModule :: SourceId -> String -> Compiler [Statement]
compileModule sid source = do
  ast        <- try $ parseBlock sid source
  rewriteAst <- try $ rewrite ast
  try $ desugarBefore rewriteAst
