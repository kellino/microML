module Compiler.CallGraph where

import MicroML.Syntax

import qualified Data.Set as S
import Data.List
import Data.Function

sortDefs :: [(String, Expr)] -> S.Set [(String, Expr)]
sortDefs = undefined

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
