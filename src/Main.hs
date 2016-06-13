module Main where

import Repl 
import Parser

main :: IO ()
-- main = repl
main = do
    res <- parseFromFile exprParser "../tests/simple"
    case res of
      Left err -> print err
      Right xs -> print xs
