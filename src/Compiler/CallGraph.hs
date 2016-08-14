module Compiler.CallGraph 
    ( checkForDuplicates
    , reachableFromMain) 
    where

import Data.List (nub, nubBy, sort, (\\))
import Data.Graph
import Data.Function (on)
import Data.Maybe (fromJust)

import MicroML.Syntax

----------------
-- DUPLICATES --
----------------

-- | if there are any duplicate definitions then abandon compilation. The line numbering function
--   is very crude and not fit for purpose. It assumes each function only occupies one line and that
--   there are no comments!

checkForDuplicates :: [(String, Expr)] -> [(String, Expr)]
checkForDuplicates code 
  | length code == length nubbed  = code
  | otherwise  = error $ red ++ "Error" ++ clear ++ ": duplicate definition of function " ++ getFuncName (length nubbed) code ++ 
      " found at line " ++ "\ESC[1m" ++ show (length nubbed) ++ "\ESC[0m. Aborting." 
  where nubbed = nubBy ((==) `on` fst . snd) numbered
        numbered :: [(Int, (String, Expr))]
        numbered = zip [1..] code

getFuncName :: Int -> [(String, Expr)] -> String
getFuncName n code = "\ESC[1m" ++ (fst . head . drop (n-1)) code ++ "\ESC[0m"

----------------------
-- UNREACHABLE CODE -- 
----------------------

doesMainExist :: [(String, Expr)] -> Either String [(String, Expr)]
doesMainExist code = if "main" `elem` map fst code then Right code else Left "no main"

putMainFirst :: [(String, Expr)] -> [(String, Expr)]
putMainFirst code = dropWhile notMain code ++ takeWhile notMain code
    where notMain (x,_) = x /= "main"

getRHSVars :: (String, Expr) -> (String, [String])
getRHSVars (nm, xs) = (nm, nub . extract . words . stripQuotes . stripParens . show $ xs)
        where stripParens = filter (\x -> x /= '(' && x /= ')')
              stripQuotes = filter (/= '\"')
              extract = go []
                  where go acc []  = acc
                        go acc [_] = acc
                        go acc (x:y:ys)
                            | x == "Var" = go (y:acc) ys
                            | otherwise = go acc (y:ys)

getTopLevel :: [(String, Expr)] -> [String]
getTopLevel = foldr (\(x, _) a -> x : a) []  

isCalled :: [(String, Expr)] -> [(String, [String])]
isCalled code = map isCalled' code
    where isCalled' ex@(x,_) = (x, filter (`elem` tops) $ snd . getRHSVars $ ex)
          tops = getTopLevel code

getOrderedNodes :: [(String, Expr)] -> [(String, [String])]
getOrderedNodes code = 
    case doesMainExist code of
         Right _ -> isCalled (putMainFirst code)
         Left  _ -> error $ redError ++ "no main function found"

buildGraph :: [(String, Expr)] -> Graph
buildGraph code = buildG (1, length code) $ concatMap (\(x, xs) -> zip (repeat x) xs) $ call mainFirst table
    where table = zip (getTopLevel mainFirst) [1..] 
          mainFirst = putMainFirst code
          call c t = map (\(x, xs) -> (tLookup x t, map (`tLookup` t) xs)) (getOrderedNodes c)
          tLookup x' t' = fromJust $ lookup x' t'

-- | the main function for the module
reachableFromMain :: [(String, Expr)] -> [(String, Expr)]
reachableFromMain cd = 
    let reach = sort $ reachable (buildGraph cd) 1
        all'   = [1..(length cd)]
     in if reach /= all'
           then error $ tellError (map fst (getOrderedNodes cd)) (all' \\ reach)
           else cd
     
-----------
-- ERROR --
-----------

tellError :: [String] -> [Int] -> String
tellError nodes unreachable = 
    let funcs = map (\x -> nodes !! (x-1)) unreachable
    in if length funcs == 1
       then redError ++ "The function " ++ bold ++ head funcs ++ clear ++ " is unreachable from main, so compilation is being abandoned."
       else redError ++ "The functions " ++ ppr funcs ++ " are unreachable from main, so compilation is being abandoned"
    where ppr funcs'
            | length funcs' == 2    = bold ++ head funcs' ++ clear ++ " and " ++ bold ++ last funcs' ++ clear
            | otherwise            = bold ++ head funcs' ++ clear ++ ", " ++ ppr funcs'

redError = red ++ "Error: " ++ clear

