module Errors.Views where

digitCount :: Int -> Int
digitCount = go 1 . abs
    where
        go ds n = if n >= 10 then go (ds + 1) (n `div` 10) else ds

renderDiagnostic :: Int
renderDiagnostic = 10
