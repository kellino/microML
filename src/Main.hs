module Main where

--import Repl 
import Parser

import System.Environment (getArgs)
import Text.Megaparsec

-- interim version for testing. Possibly to be rewritten with System.Console.GetOpt

main :: IO ()
main = undefined

{-main :: IO ()-}
{-main = do-}
    {-args <- getArgs-}
    {-if null args-}
       {-then repl-}
       {-else do -}
           {-res <- parseFromFile parseProgram (head args)-}
           {-case res of-}
             {-Left err -> print $ parseErrorPretty err-}
             {-Right xs -> print xs-}
