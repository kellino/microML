module MicroML.ListPrimitives where

import MicroML.Syntax

import qualified Data.Char as DC

enumFromTo_ :: Expr -> Expr -> Expr
enumFromTo_ (Lit (LInt a)) (Lit (LInt b))         = List $ (Lit . LInt) <$> [a .. b]
enumFromTo_ (Lit (LDouble a)) (Lit (LDouble b)) = List $ (Lit . LDouble) <$> [a..b]
enumFromTo_ (Lit (LChar a)) (Lit (LChar b))     = List $ (Lit . LChar) <$> [a..b]
enumFromTo_ _ _                                 = error "doesn't make any sense"         -- temp error message here

car :: Expr -> Expr
car (List (x:_)) = x
car (Lit (LString xs)) = Lit . LChar $ head xs
car (List []) = PrimitiveErr $ ListPrim "one cannot take the head of an empty list"

cdr :: Expr -> Expr
cdr (List [])          = PrimitiveErr $ ListPrim "one cannot take the tail of an empty list"
cdr (List [_])         = List []
cdr (List (_:xs))      = List xs
cdr (Lit (LString xs)) = Lit . LString $ tail xs

append' :: Expr -> Expr -> Expr
append' (List xs) (List ys)                   = List $ xs ++ ys
append' (Lit (LString xs)) (Lit (LString ys)) = Lit $ LString $ xs ++ ys

cons :: Expr -> Expr -> Expr
cons x1 (List []) = List [x1]
cons x1 (List [x2]) = List [x1, x2]
cons x1 (List xs) = List $ x1 : xs
cons (Lit (LChar a)) (Lit (LString as)) = Lit . LString $ a:as
cons _ _ = PrimitiveErr $ ListPrim "I suspect the second item is neither a list nor a string..."

compose :: Expr -> Expr -> Expr
compose a b = Lit $ LString $ show a ++ " " ++ show b

------------------------------------
-- STRING MANIPULATION PRIMITIVES --
------------------------------------

ord' :: Expr -> Expr
ord' (Lit (LChar a)) = Lit . LInt $ (toInteger . DC.ord) a
ord' _               = PrimitiveErr $ ListPrim "this function only works on type Char"

chr' :: Expr -> Expr
chr' (Lit (LInt n)) = Lit . LChar $ (DC.chr . fromIntegral) n
chr' _               = PrimitiveErr $ ListPrim "this function only works on type Char"
