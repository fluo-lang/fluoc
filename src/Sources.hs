module Sources where

import           Data.Map
import qualified Data.Text                     as T

data Span = Span SourceId Int Int | Eof deriving (Show, Eq)
fromPos sourceId pos = Span sourceId pos (pos+1)

newtype SourceId = SourceId Int deriving (Show, Eq)
newtype Sources = Sources (Map SourceId T.Text) deriving (Show, Eq)
