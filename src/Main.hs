module Language.Main where

import Language.Repl 


-- interim version for testing. Possibly to be rewritten with System.Console.GetOpt

main :: IO ()
main = repl (return ())
