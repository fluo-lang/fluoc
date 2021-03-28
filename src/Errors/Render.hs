module Errors.Render where

import           Control.Monad.Writer
import           Control.Monad.Reader
import           Errors.Diagnostics
import           Sources
import           Text.Printf
import qualified Data.Map                      as D
import           Data.Maybe                     ( fromJust )

getS :: DiagnosticType -> String
getS Error   = "^"
getS Warning = "~"
getS Info    = "-"

blue :: String
blue = "\x1b[34;1m"
red :: String
red = "\x1b[31;1m"
yellow :: String
yellow = "\x1b[33;1m"

getC :: DiagnosticType -> String
getC Error   = red
getC Warning = yellow
getC Info    = blue

defaultCharSet :: CharSet
defaultCharSet = CharSet "┌─" '|' '.' '=' '/' '-' '|' '\\' '-' getS

defaultConfig :: Config
defaultConfig = RenderC defaultCharSet 4 3 getC

data CharSet = CharSet
  { startS           :: String
  , borderLeftS      :: Char
  , borderBreakS     :: Char
  , noteBullet       :: Char
  , multiTopLeft     :: Char
  , multiTop         :: Char
  , multiLeft        :: Char
  , multiBottomLeft  :: Char
  , multiBottom      :: Char
  , getSeverityUnder :: DiagnosticType -> String
  }
data Config = RenderC
  { charSet      :: CharSet
  , tabWidth     :: Int
  , contextLines :: Int
  , color        :: DiagnosticType -> String
  }
data RenderEnv = RS
  { config  :: Config
  , sources :: Sources
  , filemap :: FileMap
  }

-- Render driver
type RenderD = ReaderT RenderEnv (Writer String)

type Message = String

getFilename :: SourceId -> RenderD FilePath
getFilename sid = do
  flmap <- asks filemap
  return $ fromJust $ D.lookup sid flmap

getFile :: SourceId -> RenderD String
getFile sid = do
  flmap <- asks sources
  return $ fromJust $ D.lookup sid flmap


getLineCol :: SourceId -> Int -> RenderD (Int, Int)
getLineCol sid p = do
  contents <- getFile sid
  return $ getLineColStr contents p (0, 0)

-- Top message
-- ```test
-- error[E001]: message
-- ```
renderHeader :: String -> Int -> Message -> RenderD ()
renderHeader prefix errId msg = do
  tell $ printf "%s[E%03d]: %s\n" prefix errId msg

-- Top left border and span:
-- ```test
-- ┌─ test.fl:2:4\n
-- ```
renderSnippetState :: SourceId -> Int -> RenderD ()
renderSnippetState sid p = do
  filename <- getFilename sid
  (l, c)   <- getLineCol sid p
  leading  <- asks $ startS . charSet . config
  tell leading
  tell $ printf " %s:%d:%d\n" filename (l + 1) (c + 1)

emptyLine :: RenderD ()
emptyLine = tell "\n"

runRender :: RenderD a -> RenderEnv -> (a, String)
runRender r env = runWriter $ runReaderT r env
