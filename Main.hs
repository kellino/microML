module Main  where

import Parser

main :: IO ()
main = do
    xs <- parseFromFile expr "./test"
    case xs of
      Left err -> print err
      Right a -> print a

