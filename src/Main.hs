module Main where

import Repl 

import System.Console.GetOpt
import Data.Maybe (fromMaybe)

data Flag 
    = Verbose | Version | Input String deriving Show

flags :: [OptDescr Flag]
flags = 
    [ Option ['f'] ["file"] (OptArg inp "FILE") "input file" ]

inp :: Maybe String -> Flag
inp = Input . fromMaybe "stdin"

{-parse argv = case getOpt Permute flags argv of-}
               {-(args , f, []) -> do-}
                   {-if null f-}
                      {-then -}
                      {-else return ()-}

main :: IO ()
main = repl
{-main = do-}
    {-res <- parseFromFile exprParser "../tests/simple"-}
    {-case res of-}
      {-Left err -> print err-}
      {-Right xs -> print xs-}
