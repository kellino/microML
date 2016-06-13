module ShowAST where

import Syntax

import Data.Tree
import Data.Tree.Pretty


-- takes a parsed expression, converts it to a string, and then splits it
getConstr :: Show a => a -> [String]
getConstr pExpr = words $ show pExpr

-- draw a vertical tree by default, as is normal in CS for parse trees
printTree :: Tree String -> IO ()
printTree t = putStrLn $ drawVerticalTree t

buildTree [] = []
buildTree (x:xs) = Node x : buildTree xs 

test :: Expr
test = Op Add (Num 1) (Num 2)

parseTree :: Tree String
parseTree = Node "Op" 
                [Node "Add" 
                    [Node "Num" 
                        [Node "3" []], 
                     Node "Num" 
                        [Node "4" []]]]
