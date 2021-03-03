module Sources where

import Data.Map
import qualified Data.Text as T

data Span = Span Int Int SourceId deriving (Show, Eq)
mapSpan f (Span s e id) = Span (f s) (f e) id

dummySpan :: Span
dummySpan = Span 0 1 (SourceId 0)

newtype SourceId = SourceId Int deriving (Show, Eq)

newtype Sources = Sources (Map SourceId T.Text) deriving (Show, Eq)
