module Sources where

import Data.Map
import qualified Data.Text as T

data Span = Span Int Int SourceId deriving (Show, Eq)
mapSpan f (Span s e id) = Span (f s) (f e) id

dummySpan :: Span
dummySpan = Span 0 1 (SourceId 0)

spanLoHi :: Span -> Span -> Span
spanLoHi (Span s _ _) (Span _ e id) = Span s e id

mapSpanHigh f (Span _ e id) = Span (f e - 1) (f e) id

newtype SourceId = SourceId Int deriving (Show, Eq)

newtype Sources = Sources (Map SourceId T.Text) deriving (Show, Eq)
