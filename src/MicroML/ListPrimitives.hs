module MicroML.ListPrimitives where

import MicroML.Syntax
import Control.Monad.Except
import Control.Monad.Identity

import qualified Data.Char as DC

type PrimError a = ExceptT ErrorMsg Identity a

runPrimError :: ExceptT e Identity a -> Either e a
runPrimError ev = runIdentity $ runExceptT ev

enumFromTo_ :: Expr -> Expr -> Expr
enumFromTo_ (Lit (LInt a)) (Lit (LInt b)) = foldr (BinOp OpCons) Nil $ (Lit . LInt ) <$> [a .. b]
enumFromTo_ (Lit (LDouble a)) (Lit (LDouble b)) = foldr (BinOp OpCons) Nil $ (Lit . LDouble ) <$> [a .. b]
enumFromTo_ (Lit (LChar a)) (Lit (LChar b))     = foldr (BinOp OpCons) Nil $ (Lit . LChar) <$> [a .. b]
enumFromTo_ _ _                                 = PrimitiveErr $ ListPrim ""

car :: Expr -> PrimError Expr 
car Nil = throwError "you're trying to perform Car on an empty list!"
car (BinOp OpCons x _) =  return x

cdr :: Expr -> Expr
cdr Nil = PrimitiveErr $ ListPrim "there is no head of an empty list!"
cdr (BinOp OpCons _ xs) = xs
cdr _   = error "you can only take the tail of a list"

append :: Expr -> Expr -> Expr
append xs Nil                               = xs
append (BinOp OpCons x Nil) xs@(BinOp OpCons _ _) = BinOp OpCons x xs
append (BinOp OpCons x xs) ys                  = BinOp OpCons x (append xs ys)
append _ _                                  = PrimitiveErr $ ListPrim "one of your two objects isn't a list"

------------------------------------
-- STRING MANIPULATION PRIMITIVES --
------------------------------------

ord' :: Expr -> Expr
ord' (Lit (LChar a)) = Lit . LInt $ (toInteger . DC.ord) a
ord' _               = PrimitiveErr $ ListPrim "this function only works on type Char"

chr' :: Expr -> Expr
chr' (Lit (LInt n)) = Lit . LChar $ (DC.chr . fromIntegral) n
chr' _               = PrimitiveErr $ ListPrim "this function only works on type Char"
