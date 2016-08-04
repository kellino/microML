module Compiler.CallGraph where

import MicroML.Syntax

import Data.List
import Data.Function

checkForDuplicates :: [(String, Expr)] -> [(String, Expr)]
checkForDuplicates code 
  | length code == length nubbed  = code
  | otherwise  = error $ "\ESC[31mError: \ESC[0mduplicate definition of function " ++ getFuncName (length nubbed) code ++ 
      " found at line " ++ "\ESC[1m" ++ show (length nubbed) ++ "\ESC[0m. Aborting." 
  where nubbed = nubBy ((==) `on` fst . snd) numbered
        numbered :: [(Int, (String, Expr))]
        numbered = zip [1..] code

getFuncName :: Int -> [(String, Expr)] -> String
getFuncName n code = "\ESC[1m" ++ (fst . head . drop (n-1)) code ++ "\ESC[0m"

pruneUnused :: [(String, Expr)] -> [(String, Expr)] -> [(String, Expr)]
pruneUnused _ [] = []
pruneUnused [] acc = acc
pruneUnused (x:xs) acc 
    | ("App " ++ fst x) `isInfixOf` concatMap (show . snd) xs = pruneUnused xs (x:acc) 
    | otherwise                                    = error "Unused function definition"
