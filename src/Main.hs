module Main  where

import Repl.Repl
-- import System.Environment (getArgs)


main :: IO ()
main = shell

{-main :: IO ()-}
{-main = do-}
  {-args <- getArgs-}
  {-case args of-}
    {-[]      -> shell (return ())-}
    {-[fname] -> shell (using [fname])-}
    {--- ["test", fname] -> shell (using [fname] >> browse [] >> quit ())-}
    {-_ -> putStrLn "invalid arguments"-}
