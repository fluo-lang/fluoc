{-# LANGUAGE ViewPatterns, PatternSynonyms #-}
module Helpers where

import           Data.Text

pattern (:>) :: Char -> Text -> Text
pattern x :> xs <- (uncons -> Just (x, xs))
pattern Empty :: Text
pattern Empty <- (uncons -> Nothing)
