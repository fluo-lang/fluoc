module Sources where

import           Data.Map
import qualified Data.Text                     as T

data Span = Span SourceId Int Int
  deriving (Show, Eq)
mapSpan f (Span id s e) = Span id (f s) (f e)

dummySpan :: Span
dummySpan = Span (SourceId 0) 0 1

spanLoHi :: Span -> Span -> Span
spanLoHi (Span s _ _) (Span _ e id) = Span s e id

mapSpanHigh f (Span id _ e) = Span id (f e - 1) (f e)

newtype SourceId = SourceId Int deriving (Show, Eq)

newtype Sources = Sources (Map SourceId T.Text) deriving (Show, Eq)
