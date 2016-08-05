module Repl.HelpDicts where

import qualified Data.Map as Map

standard :: Map.Map String String
standard = Map.fromList 
    [ ("compose", "\n       ***     compose     ***\n\n" ++
                  "** compose :: (b -> c) -> (a -> b) -> a -> c**\n" ++ 
                  "compose takes two __unary__ functions and links the results together\n" ++
                  "like this: a to a1 to a2\n" ++
                  "#Example:#\n" ++ "    compose succ succ 3 to 5\n" ++
                  "It works like this\n result = 5 <- (succ 4 = 5) <- (succ 3 = 4) <- 3\n\n") 
      , ("unary", "\nhaving only one parameter, like **id (id :: a -> a)**, so id 5 = 5\n\n")
      , ("succ",  "\n    ***     succ    ***\n\n" ++ 
                  "** succ :: Number -> Number**\n" ++
                  "#Example:#\n" ++ "    succ 3 = 4\n\n")
      , ("help", "\n        ***     Help    ***         \n\n" ++
                 "**Welcome to microML help**\n\n"
        )
    ] 
