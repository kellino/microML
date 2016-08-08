module Compiler.CallGraph 
    (checkForDuplicates, validDefs) where

import MicroML.Syntax

import Data.List
import Data.Function

import Control.Arrow (second)

----------------
-- DUPLICATES --
----------------

-- | if there are any duplicate definitions then abandon compilation. The line numbering function
-- is very crude and not fit for purpose. It assumes each function only occupies one line and that
-- there are no comments!

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

----------------------
-- UNREACHABLE CODE --
----------------------

-- | While is seems a bit mean to abandon compilation because of an unused definition, microML is
-- primarily intended as a teaching language and should therefore be a little stricter on these
-- issues. It is more likely that a student has simply forgotten to call a function rather than
-- to have written something without any obvious use or for future development purposes.

-- removes duplicate vars from an expression
vars :: Expr -> [String]
vars = nub . getVars 
    where getVars :: Expr -> [String]
          getVars ex = go [] (words . show $ ex)
            where 
                go acc []  = acc
                go acc [_] = acc
                go acc (x:y:xs)
                  | x == "Var"  || x == "(Var"  = go (y:acc) xs
                  | otherwise    = go acc (y:xs)

calledVars :: [(String, Expr)] -> [String]
calledVars code = map (filter (/= ')')) $ concatMap (snd . second vars) code

-- | if there is no main, then abandon immediately
doesMainExist :: [(String, Expr)] -> Bool
doesMainExist ex = "main" `elem` map fst ex

-- | is a function called on the rhs of an equation?
isCalled :: String -> [String] -> Bool
isCalled lhs rhs = show lhs `elem` rhs

topLevelVars :: [(String, Expr)] -> [String]
topLevelVars = map fst 

callable :: [String] -> [(String, Expr)] -> [String] -> [String]
callable [] _ acc = acc 
callable _ [] acc = acc
callable (x:xs) code acc 
  | isCalled x code'   = callable xs code (x:acc)
  | otherwise          = callable xs code acc
  where code' = calledVars code

toCompile :: [(String, Expr)] -> [String]
toCompile code = "main" : callable (topLevelVars code) code []

unused :: [(String, Expr)] -> [String]
unused code = topLevelVars code \\ toCompile code

validDefs :: [(String, Expr)] -> [(String, Expr)]
validDefs code =
    if not $ doesMainExist code
       then error "\ESC[31;1mError:\ESC[0m A \ESC[31;1mmain function\ESC[0m has not been defined, so abandoning compilation. Please write a main function."
       else vds code

vds :: [(String, Expr)] -> [(String, Expr)]
vds code 
   | not (null duds)  = error $ "\ESC[31;1mError:\ESC[0m the following function" ++ plural ++ " defined but not reachable: " ++ join duds
   | otherwise                 = code
   where duds = unused code
         plural = if length duds == 1 then " is" else "s are"
         join s = if length s == 1 then head duds else intercalate ", " duds
