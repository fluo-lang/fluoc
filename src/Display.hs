module Display where

class Display a where
  display :: a -> String

instance Display Char where
  display c = "character `" ++ [c] ++ "`"
