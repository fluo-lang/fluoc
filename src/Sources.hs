module Sources where

import Data.Map
import qualified Data.Text as T

data Span = Span Int Int SourceId
mapSpan f (Span s e id) = Span (f s) (f e) id

newtype SourceId = SourceId Int

newtype Sources = Sources (Map SourceId T.Text)
