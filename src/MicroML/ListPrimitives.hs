{-# LANGUAGE FlexibleContexts #-}

module MicroML.ListPrimitives  where

import MicroML.Syntax
import Control.Monad.Except
import qualified Data.Text.Lazy as L

type ThrowsError = Except MLError

car :: Expr -> Expr
car (List xs) = head xs
car (List []) = error "head of empty list"

cdr :: Expr -> Expr
cdr (List [])     = error "empty list"
cdr (List [x])    = List []
cdr (List (x:xs)) = List xs

cons :: Expr -> Expr -> Expr
cons x1 (List []) = List [x1]
cons x1 (List [x2]) = List [x1, x2]
cons x1 (List xs) = List $ x1 : xs
-- cons _ _ = throwError $ Default $ L.pack "illegal cons operation: are you sure the second element is a list?"

init' :: Expr -> ThrowsError Expr
init' (List []) = throwError $ Default $ L.pack "the beginning of an empty list doesn't make any sense"
init' (List xs) = return $ List $ init xs
