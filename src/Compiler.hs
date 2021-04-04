{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Compiler where

import           Control.Monad.State            ( StateT
                                                , MonadIO
                                                , MonadState
                                                , runStateT
                                                )
import           Control.Monad.Morph            ( hoist
                                                , generalize
                                                )
import           Sources
import           Errors.Diagnostics
import           Data.Map                       ( empty )
import           Control.Monad.Except           ( ExceptT
                                                , runExceptT
                                                , Except
                                                )
import           Errors.Views                   ( renderDiagnostics )
import           Errors.Render                  ( runRender
                                                , defaultConfig
                                                , RenderEnv(..)
                                                )
import           Data.List.Split                ( splitOn )
import qualified Data.Sequence                 as S

data CompilerState = CST
  { sourceMap :: Sources
  , fileMap   :: FileMap
  , currId    :: SourceId
  }

newtype Compiler a = Compiler {
                              runC :: ExceptT Diagnostics (StateT CompilerState IO) a
                            } deriving (Functor, Applicative, Monad, MonadIO, MonadState CompilerState)

mkEnv :: CompilerState -> RenderEnv
mkEnv (CST sMap fMap _) =
  RS defaultConfig sMap fMap (S.fromList . splitOn "\n" <$> sMap)

showError' :: CompilerState -> Diagnostics -> IO ()
showError' env ds = do
  putStr . snd $ runRender (renderDiagnostics ds) $ mkEnv env

runCompiler :: Compiler a -> IO (Maybe a, CompilerState)
runCompiler c = do
  res <- runStateT (runExceptT $ runC c) state
  case res of
    (Right a  , s) -> return (Just a, s)
    (Left  err, s) -> do
      showError' s err
      return (Nothing, s)
  where state = CST Data.Map.empty Data.Map.empty (SourceId 0)

try :: Except Diagnostics b -> Compiler b
try e = Compiler $ hoist generalize e
