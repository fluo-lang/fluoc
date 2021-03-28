{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Compiler where

import           Control.Monad.State            ( StateT
                                                , MonadIO
                                                , MonadState
                                                , runStateT
                                                )
import           Sources
import           Data.Map                       ( empty )

data CompilerState = CST
  { sourceMap :: Sources
  , fileMap   :: FileMap
  , currId    :: SourceId
  }

newtype Compiler a = Compiler {
                              runC :: StateT CompilerState IO a
                            } deriving (Functor, Applicative, Monad, MonadIO, MonadState CompilerState)

runCompiler :: Compiler a -> IO (a, CompilerState)
runCompiler c = runStateT (runC c) state
  where state = CST Data.Map.empty Data.Map.empty (SourceId (-1))
