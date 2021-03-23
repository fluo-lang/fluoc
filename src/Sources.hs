module Sources where

import           Data.Map
import qualified Data.Text                     as T

data Span = Span SourceId Int Int | Eof deriving (Show, Eq)
fromPos sourceId pos = Span sourceId pos (pos+1)

btwn (Span sid s _) (Span _ _ e) = Span sid s e

bt :: (Spanned a, Spanned b) => a -> b -> Span
bt t1 t2 = btwn (getSpan t1) (getSpan t2)

class Spanned a where
  getSpan :: a -> Span
  setSpan :: Span -> a -> a

newtype SourceId = SourceId Int deriving (Show, Eq)
newtype Sources = Sources (Map SourceId T.Text) deriving (Show, Eq)
