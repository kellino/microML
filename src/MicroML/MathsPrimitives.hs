module MicroML.MathsPrimitives where

import MicroML.Syntax
import Data.List (isInfixOf)
import Data.List.Split (splitOn)
import Data.Bits (xor)

add :: Expr -> Expr -> Expr
add (Lit (LInt a)) (Lit (LInt b)) = Lit $ LInt $ a + b
add (Lit (LDouble a)) (Lit (LDouble b)) = Lit $ LDouble $ a + b
add (Lit (LInt a)) (Lit (LDouble b)) = Lit $ LDouble $ realToFrac a + b
add (Lit (LDouble a)) (Lit (LInt b)) = Lit $ LDouble $ a + realToFrac b

or', and', xor' :: Expr -> Expr -> Expr
or' (Lit (LBoolean a)) (Lit (LBoolean b)) = Lit $ LBoolean $ a || b
and' (Lit (LBoolean a)) (Lit (LBoolean b)) = Lit $ LBoolean $ a && b
xor' (Lit (LBoolean a)) (Lit (LBoolean b)) = Lit $ LBoolean $ a `xor` b

sub :: Expr -> Expr -> Expr
sub (Lit (LInt a)) (Lit (LInt b)) = Lit $ LInt $ a - b
sub (Lit (LDouble a)) (Lit (LDouble b)) = Lit $ LDouble $ a - b
sub (Lit (LInt a)) (Lit (LDouble b)) = Lit . LDouble . truncate' $ realToFrac a - b
sub (Lit (LDouble a)) (Lit (LInt b)) = Lit . LDouble . truncate' $ a - realToFrac b
sub _ _ = PrimitiveErr $ MathsPrim "perhaps you meant (-)"

mul :: Expr -> Expr -> Expr
mul (Lit (LInt a)) (Lit (LInt b)) = Lit $ LInt $ a * b
mul (Lit (LDouble a)) (Lit (LDouble b)) = Lit . LDouble . truncate' $ a * b
mul (Lit (LInt a)) (Lit (LDouble b)) = Lit . LDouble . truncate' $ realToFrac a * b
mul (Lit (LDouble a)) (Lit (LInt b)) = Lit . LDouble . truncate' $ a * realToFrac b

div' :: Expr -> Expr -> Expr
div' (Lit (LInt a)) (Lit (LInt b)) = Lit $ LDouble $ realToFrac a / realToFrac b
div' (Lit (LDouble a)) (Lit (LDouble b)) = Lit $ LDouble $ a / b
div' (Lit (LInt a)) (Lit (LDouble b)) = Lit $ LDouble $ realToFrac a / b
div' (Lit (LDouble a)) (Lit (LInt b)) = Lit $ LDouble $ a / realToFrac b

intDiv :: Expr -> Expr -> Expr
intDiv (Lit (LInt a)) (Lit (LInt b)) = Lit . LInt $ a `div` b
intDiv (Lit (LDouble a)) (Lit (LDouble b)) = Lit $ LInt $ floor a `div` floor b
intDiv (Lit (LInt a)) (Lit (LDouble b)) = Lit $ LInt $ a `div` floor b
intDiv (Lit (LDouble a)) (Lit (LInt b)) = Lit $ LInt $ floor a `div` b

mod' :: Expr -> Expr -> Expr
mod' (Lit (LInt a)) (Lit (LInt b)) = Lit $ LInt $ a `mod` b
mod' _ _ = error "only works on integers" -- improve this 

exp' :: Expr -> Expr -> Expr
exp' (Lit (LInt a)) (Lit (LInt b)) = Lit $ LInt $ a^b
exp' (Lit (LInt a)) (Lit (LDouble b)) = Lit $ LDouble $ realToFrac a**b
exp' (Lit (LDouble a)) (Lit (LInt b)) = Lit $ LDouble $ a ^ b
exp' (Lit (LDouble a)) (Lit (LDouble b)) = Lit $ LDouble $ a**b

log' :: Expr -> Expr 
log' (Lit (LInt a))    = Lit . LDouble $ log $ realToFrac a
log' (Lit (LDouble a)) = Lit . LDouble $ log a

opEq :: Expr -> Expr -> Expr
opEq (Lit (LInt a)) (Lit (LDouble b)) = Lit . LBoolean $ realToFrac a == b
opEq (Lit (LDouble a)) (Lit (LInt b)) = Lit . LBoolean $ a == realToFrac b
opEq a b = Lit . LBoolean $ a == b

opLe :: Expr -> Expr -> Expr
opLe (Lit (LInt a)) (Lit (LDouble b)) = Lit . LBoolean $ realToFrac a <= b
opLe (Lit (LDouble a)) (Lit (LInt b)) = Lit . LBoolean $ a <= realToFrac b
opLe a b = Lit . LBoolean $ a <= b

opLt :: Expr -> Expr -> Expr
opLt (Lit (LInt a)) (Lit (LDouble b)) = Lit . LBoolean $ realToFrac a < b
opLt (Lit (LDouble a)) (Lit (LInt b)) = Lit . LBoolean $ a < realToFrac b
opLt a b = Lit . LBoolean $ a < b

opGt :: Expr -> Expr -> Expr
opGt (Lit (LInt a)) (Lit (LDouble b)) = Lit . LBoolean $ realToFrac a > b
opGt (Lit (LDouble a)) (Lit (LInt b)) = Lit . LBoolean $ a > realToFrac b
opGt a b = Lit . LBoolean $ a > b

opGe :: Expr -> Expr -> Expr
opGe (Lit (LInt a)) (Lit (LDouble b)) = Lit . LBoolean $ realToFrac a >= b
opGe (Lit (LDouble a)) (Lit (LInt b)) = Lit . LBoolean $ a >= realToFrac b
opGe a b = Lit . LBoolean $ a >= b

opNotEq :: Expr -> Expr -> Expr
opNotEq (Lit (LInt a)) (Lit (LDouble b)) = Lit . LBoolean $ realToFrac a /= b
opNotEq (Lit (LDouble a)) (Lit (LInt b)) = Lit . LBoolean $ a /= realToFrac b
opNotEq a b = Lit . LBoolean $ a /= b

-- | an abritrary truncation of floating-point rounding errors. It's unlikely that this will be a
-- problem for students. Horrible horrible code though. 

-- TODO find an elegant mathematical solution to this problem, rather than nasty string manipulation
truncate' :: Double -> Double
truncate' = read . dropZeros . show
    where dropZeros x = head (split x) ++ "." ++ getValid (head (tail (split x)))
          split       = splitOn "."
          getValid s 
              | "e" `isInfixOf` s  = s
              | hasform s = if length s == 1 then s else  show $ (read [head s] :: Int) + 1
              | take 3 s   == "000" = "0"
              | otherwise  = head s : getValid (tail s) 

hasform :: String -> Bool
hasform (_:ys) = all (== '9') ys 
