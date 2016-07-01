module Language.ListPrimitives  where

import Language.Syntax
import Control.Monad.Except
-- import qualified Data.Text as T

-- type ErrorMonad = Either MLError 

-- car :: Expr -> ErrorMonad Expr
car (List (x:_)) = x
-- car (List []) = throwError $ Default $ T.pack "empty list"

cdr :: Expr -> Expr
cdr (List (_:xs)) = List xs

--cons :: Expr -> Expr -> Either MLError Expr
cons x1 (List []) = List [x1]
cons x1 (List [x2]) = List [x1, x2]
cons x1 (List xs) = List $ x1 : xs
--cons _ _ = throwError $ Default $ T.pack "illegal cons operation: are you sure the second element is a list?"

init' :: Expr -> Expr
init' (List []) = error "nothing"
init' (List xs) = List $ init xs
