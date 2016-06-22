module Main where

import Repl 


-- interim version for testing. Possibly to be rewritten with System.Console.GetOpt

main :: IO ()
main = repl (return ())
