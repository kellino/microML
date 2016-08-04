module Repl.HelpDicts where

import qualified Data.Map as Map

standard :: Map.Map String String
standard = Map.fromList 
    [ ("compose", "\n         ***compose***       \n\n" ++
                  "** compose :: (b -> c) -> (a -> b) -> a -> c**\n" ++ 
                  "compose takes two unary functions and links the results together\n" ++
                  "like this: a to a1 to a2\n" ++
                  "#Example:#\n" ++ "    compose succ succ 3 to 5\n" ++
                  "It works like this\n result = 5 <- (succ 4 = 5) <- (succ 3 = 4) <- 3\n\n") 
    , ("unary", "\nhaving only one parameter\n")
    ] 
