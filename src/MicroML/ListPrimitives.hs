module MicroML.ListPrimitives  where

import MicroML.Syntax
import Control.Monad.Except
import qualified Data.Text.Lazy as L

type ThrowsError = Either MLError

car :: Expr -> ThrowsError Expr
car (List [x]) = return x
car (List (x:_)) = return x
car (List []) = throwError $ Default $ L.pack "empty list"

cdr :: Expr -> ThrowsError Expr
cdr (List []) = throwError $ Default $ L.pack "cannot take the tail of an empty list"
cdr (List [_]) = return $ List []
cdr (List (_:xs)) = return $ List xs
cdr _ = throwError $ Default $ L.pack "bad arg"

cons :: Expr -> Expr -> Expr
cons x1 (List []) = List [x1]
cons x1 (List [x2]) = List [x1, x2]
cons x1 (List xs) = List $ x1 : xs
-- cons _ _ = throwError $ Default $ L.pack "illegal cons operation: are you sure the second element is a list?"

init' :: Expr -> ThrowsError Expr
init' (List []) = throwError $ Default $ L.pack "the beginning of an empty list doesn't make any sense"
init' (List xs) = return $ List $ init xs
