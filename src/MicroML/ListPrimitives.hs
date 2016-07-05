module MicroML.ListPrimitives 
    (car
   , cdr
   , cons
   , init'
   , compose
   , append') where

import MicroML.Syntax


-- TODO add proper exception handling

car :: Expr -> Expr
car (List (x:_))  = x
car (List [])     = error "head of empty list"

cdr :: Expr -> Expr
cdr (List [])     = error "empty list"
cdr (List [_])    = List []
cdr (List (_:xs)) = List xs

append' :: Expr -> Expr -> Expr
append' (List xs) (List ys) = List $ xs ++ ys
append' (Lit (LString xs)) (Lit (LString ys)) = Lit $ LString $ xs ++ ys

init' :: Expr -> Expr
init' (List [])         = error "emtpy list"
init' (List [_])        = List []
init' (List xs)         = List $ init xs
init' (Lit (LString xs)) = Lit $ LString $ init xs

cons :: Expr -> Expr -> Expr
cons x1 (List []) = List [x1]
cons x1 (List [x2]) = List [x1, x2]
cons x1 (List xs) = List $ x1 : xs

compose :: Expr -> Expr -> Expr
compose a b = Lit $ LString $ show a ++ " " ++ show b
