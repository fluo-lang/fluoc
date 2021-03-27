module Sources where

import           Data.Map
import qualified Data.Text                     as T

data Span = Span SourceId Int Int | Eof deriving (Show, Eq)
fromPos :: SourceId -> Int -> Span  
fromPos sourceId pos = Span sourceId pos (pos+1)

btwn :: Span -> Span -> Span   
btwn (Span sid s _) (Span _ _ e) = Span sid s e
btwn _ _ = Eof

gap :: Span -> Span -> Span   
gap (Span sid _ e) (Span _ s _) = Span sid e s
gap _ _ = Eof

bt :: (Spanned a, Spanned b) => a -> b -> Span
bt t1 t2 = btwn (getSpan t1) (getSpan t2)

class Spanned a where
  getSpan :: a -> Span

newtype SourceId = SourceId Int deriving (Show, Eq)
newtype Sources = Sources (Map SourceId T.Text) deriving (Show, Eq)
