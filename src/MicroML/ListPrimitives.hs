module MicroML.ListPrimitives where

import MicroML.Syntax

import qualified Data.Char as DC

enumFromTo_ :: Expr -> Expr -> Expr
enumFromTo_ (Lit (LInt a)) (Lit (LInt b)) = foldr (BinOp OpCons) Nil $ (Lit . LInt ) <$> [a .. b]
enumFromTo_ (Lit (LDouble a)) (Lit (LDouble b)) = foldr (BinOp OpCons) Nil $ (Lit . LDouble ) <$> [a .. b]
enumFromTo_ (Lit (LChar a)) (Lit (LChar b))     = foldr (BinOp OpCons) Nil $ (Lit . LChar) <$> [a .. b]
enumFromTo_ _ _                                 = PrimitiveErr $ ListPrim ""

car :: Expr -> Expr 
car (BinOp OpCons x _) = x

cdr :: Expr -> Expr
cdr (BinOp OpCons _ xs) = xs

append :: Expr -> Expr -> Expr
append xs Nil                               = xs
append (BinOp OpCons x Nil) xs@(BinOp OpCons _ _) = BinOp OpCons x xs
append (BinOp OpCons x xs) ys                  = BinOp OpCons x (append xs ys)
append _ _                                  = PrimitiveErr $ ListPrim "one of your two objects isn't a list"

------------------------------------
-- STRING MANIPULATION PRIMITIVES --
------------------------------------

show' :: Expr -> Expr
show' str@(Lit (LString _)) = str
show' (Lit (LInt x))        = Lit . LString $ show x
show' (Lit (LDouble x))     = Lit . LString $ show x
show' (Lit (LChar x))       = Lit . LString $ show x
show' (Lit (LBoolean x))    = Lit . LString $ show x

read' :: Expr -> Expr
read' int@(Lit (LInt _)) = int
read' doub@(Lit (LDouble _)) = doub
read' (Lit (LString x)) = Lit . LInt $ read x

ord' :: Expr -> Expr
ord' (Lit (LChar a)) = Lit . LInt $ (toInteger . DC.ord) a
ord' _               = PrimitiveErr $ ListPrim "this function only works on type Char"

chr' :: Expr -> Expr
chr' (Lit (LInt n)) = Lit . LChar $ (DC.chr . fromIntegral) n
chr' _               = PrimitiveErr $ ListPrim "this function only works on type Char"
