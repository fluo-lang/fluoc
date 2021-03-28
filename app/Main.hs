module Main where

import Pipeline (pipeline, runCompiler)

main :: IO ()
main = () <$ runCompiler pipeline
