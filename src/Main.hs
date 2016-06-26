module Main where

import Language.Repl 

main :: IO ()
main = repl (return ())
--main = repl (using ["src/Libs/standard.mml"])
