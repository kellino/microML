module MicroML.ListPrimitives where

import MicroML.Syntax

import qualified Data.Char as DC

enumFromTo_ :: Expr -> Expr -> Expr
{-enumFromTo_ (Lit (LInt a)) (Lit (LInt b))         = (Lit . LInt) <$> [a .. b]-}
{-enumFromTo_ (Lit (LDouble a)) (Lit (LDouble b)) = List $ (Lit . LDouble) <$> [a..b]-}
{-enumFromTo_ (Lit (LChar a)) (Lit (LChar b))     = List $ (Lit . LChar) <$> [a..b]-}
enumFromTo_ _ _                                 = error "doesn't make any sense"         -- temp error message here

car :: Expr -> Expr 
car Nil = error "what are you thinking?"
car (Op OpCons x _) =  x

cdr :: Expr -> Expr
cdr Nil = error "nope, can't do that"
cdr (Op OpCons _ xs) = xs

------------------------------------
-- STRING MANIPULATION PRIMITIVES --
------------------------------------

ord' :: Expr -> Expr
ord' (Lit (LChar a)) = Lit . LInt $ (toInteger . DC.ord) a
ord' _               = PrimitiveErr $ ListPrim "this function only works on type Char"

chr' :: Expr -> Expr
chr' (Lit (LInt n)) = Lit . LChar $ (DC.chr . fromIntegral) n
chr' _               = PrimitiveErr $ ListPrim "this function only works on type Char"
