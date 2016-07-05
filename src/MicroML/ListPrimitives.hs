module MicroML.ListPrimitives 
    (car
   , cdr
   , cons
   , init') where

import MicroML.Syntax


-- TODO add proper exception handling

car :: Expr -> Expr
car (List (x:_))  = x
car (List [])     = error "head of empty list"

cdr :: Expr -> Expr
cdr (List [])     = error "empty list"
cdr (List [_])    = List []
cdr (List (_:xs)) = List xs

init' :: Expr -> Expr
init' (List [])   = error "emtpy list"
init' (List [_])  = List []
init' (List xs)   = List $ init xs

cons :: Expr -> Expr -> Expr
cons x1 (List []) = List [x1]
cons x1 (List [x2]) = List [x1, x2]
cons x1 (List xs) = List $ x1 : xs
