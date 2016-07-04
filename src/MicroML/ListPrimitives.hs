{-# LANGUAGE FlexibleContexts #-}

module MicroML.ListPrimitives  where

import MicroML.Syntax
import Control.Monad.Except

type ThrowsError = Except MLError

-- add proper exception handling

car :: Expr -> Expr
car (List (x:_))  = x
car (List [])     = error "head of empty list"

cdr :: Expr -> Expr
cdr (List [])     = error "empty list"
cdr (List [_])    = List []
cdr (List (_:xs)) = List xs

cons :: Expr -> Expr -> Expr
cons x1 (List []) = List [x1]
cons x1 (List [x2]) = List [x1, x2]
cons x1 (List xs) = List $ x1 : xs
-- these are just here for testing
cons _ (Var _) = Lit $ LString "still have problems with vars"
cons (Var _) _ = Lit $ LString "var at the beginning" 
cons _ _ = Lit $ LString "weird shit!"
