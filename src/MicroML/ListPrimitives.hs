module MicroML.ListPrimitives where

import MicroML.Syntax

import qualified Data.Char as DC

-- | doesn't work yet for vars
enumFromTo_ :: Expr -> Expr -> Expr
enumFromTo_ (Lit (LInt a)) (Lit (LInt b)) = foldr (BinOp OpCons) Nil $ (Lit . LInt ) <$> [a .. b]
enumFromTo_ (Lit (LDouble a)) (Lit (LDouble b)) = foldr (BinOp OpCons) Nil $ (Lit . LDouble ) <$> [a .. b]
enumFromTo_ (Lit (LChar a)) (Lit (LChar b))     = foldr (BinOp OpCons) Nil $ (Lit . LChar) <$> [a .. b]
enumFromTo_ _ _                                 = PrimitiveErr $ ListPrim ""

car :: Expr -> Expr 
car (BinOp OpCons x _) = x
car (Lit (LString x)) = Lit . LChar $ head x

cdr :: Expr -> Expr
cdr (BinOp OpCons _ xs) = xs
cdr (Lit (LString xs)) = Lit . LString $ tail xs

cons :: Expr -> Expr -> Expr
cons a Nil = BinOp OpCons a Nil
cons a (BinOp OpCons x Nil) = BinOp OpCons a (BinOp OpCons x Nil)
cons a ls@(BinOp OpCons _ _) = BinOp OpCons a ls
cons (Lit (LChar x)) (Lit (LString y)) = Lit . LString $ x : y
cons x y = error $ show x ++ " " ++ show y

append :: Expr -> Expr -> Expr
append xs Nil                                     = xs
append (BinOp OpCons x Nil) xs@(BinOp OpCons _ _) = BinOp OpCons x xs
append (BinOp OpCons x xs) ys                     = BinOp OpCons x (append xs ys)
append (Lit (LString x)) (Lit (LString y))        = Lit . LString $ x ++ y
append Nil xs                                     = xs
append x y                                        = PrimitiveErr $ ListPrim $ show x ++ " " ++ show y

------------------------------------
-- STRING MANIPULATION PRIMITIVES --
------------------------------------

show' :: Expr -> Expr
show' str@(Lit (LString _))  = str
show' (Lit (LInt x))         = Lit . LString $ show x
show' (Lit (LDouble x))      = Lit . LString $ show x
show' (Lit (LChar x))        = Lit . LString $ [x]
show' (Lit (LBoolean True))  = Lit (LString "true")
show' (Lit (LBoolean False)) = Lit (LString "false")
show' _                      = PrimitiveErr $ ListPrim "this is not a Showable object"

-- this doesn't handle binary, octal or hex yet
read' :: Expr -> Expr
read' (Lit (LString x)) = 
    let removeDot = filter (/= '.')
     in if all DC.isNumber $ removeDot x 
           then if '.' `elem` x
                then Lit . LDouble $ read x
                else Lit . LInt $ read x
           else PrimitiveErr $ ListPrim "the string does not contain a number"

ord' :: Expr -> Expr
ord' (Lit (LChar a)) = Lit . LInt $ (toInteger . DC.ord) a
ord' _               = PrimitiveErr $ ListPrim "this function only works on type Char"

chr' :: Expr -> Expr
chr' (Lit (LInt n)) = Lit . LChar $ (DC.chr . fromIntegral) n
chr' _               = PrimitiveErr $ ListPrim "this function only works on type Char"
