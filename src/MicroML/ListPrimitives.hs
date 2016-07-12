module MicroML.ListPrimitives where

import MicroML.Syntax

import qualified Data.Char as DC

-- TODO add proper exception handling

-- used for implementing [? to ?] ranges. Not as flexible as the real implementation in haskell
enumFromTo_ :: Expr -> Expr -> Expr
enumFromTo_ (Lit (LInt a)) (Lit (LInt b))         = List $ (Lit . LInt) <$> [a .. b]
enumFromTo_ (Lit (LDouble a)) (Lit (LDouble b)) = List $ (Lit . LDouble) <$> [a..b]
enumFromTo_ (Lit (LChar a)) (Lit (LChar b))     = List $ (Lit . LChar) <$> [a..b]
enumFromTo_ _ _                                 = error "doesn't make any sense"         -- temp error message here

car :: Expr -> Expr
car (List (x:_))  = x
car (Lit (LString xs)) = Lit. LChar $ head xs
car (List [])     = error "head of empty list"

cdr :: Expr -> Expr
cdr (List [])          = error "empty list"
cdr (List [_])         = List []
cdr (List (_:xs))      = List xs
cdr (Lit (LString xs)) = Lit . LString $ tail xs

append' :: Expr -> Expr -> Expr
append' (List xs) (List ys)                   = List $ xs ++ ys
append' (Lit (LString xs)) (Lit (LString ys)) = Lit $ LString $ xs ++ ys

init' :: Expr -> Expr
init' (List [])          = error "emtpy list"
init' (List [_])         = List []
init' (List xs)          = List $ init xs
init' (Lit (LString xs)) = Lit $ LString $ init xs

cons :: Expr -> Expr -> Expr
cons x1 (List []) = List [x1]
cons x1 (List [x2]) = List [x1, x2]
cons x1 (List xs) = List $ x1 : xs
cons _ _ = error ""

compose :: Expr -> Expr -> Expr
compose a b = Lit $ LString $ show a ++ " " ++ show b

------------------------------------
-- STRING MANIPULATION PRIMITIVES --
------------------------------------

toLower :: Expr -> Expr
toLower (Lit (LChar x)) = Lit . LChar $ DC.toLower x
toLower _ = error "type mismatch"

toUpper :: Expr -> Expr
toUpper (Lit (LChar x)) = Lit . LChar $ DC.toUpper x
toUpper _ = error "type mismatch"
