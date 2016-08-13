module Compiler.CallGraph where

import Data.List (nub, nubBy)
import Data.Function (on)

import MicroML.Syntax


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

makeCallGraph :: [(String, Expr)] -> [(String, [String])]
makeCallGraph code = 
    case doesMainExist code of
         Right _ -> isCalled (putMainFirst code)
         Left  _ -> error "no main function found"
