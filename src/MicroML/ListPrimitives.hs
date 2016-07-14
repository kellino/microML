module MicroML.ListPrimitives where

import MicroML.Syntax

import qualified Data.Char as DC
import Data.Maybe (fromMaybe)

-- TODO add proper exception handling

-- used for implementing [? to ?] ranges. Not as flexible as the real implementation in haskell
enumFromTo_ :: Expr -> Expr -> Expr
enumFromTo_ (Lit (LInt a)) (Lit (LInt b))         = List $ (Lit . LInt) <$> [a .. b]
enumFromTo_ (Lit (LDouble a)) (Lit (LDouble b)) = List $ (Lit . LDouble) <$> [a..b]
enumFromTo_ (Lit (LChar a)) (Lit (LChar b))     = List $ (Lit . LChar) <$> [a..b]
enumFromTo_ _ _                                 = error "doesn't make any sense"         -- temp error message here

car :: Expr -> Expr
car xs = do
    let res = car' xs
    fromMaybe (PrimitiveErr . ListPrim $ "head of an emtpy list") res

car' :: Expr -> Maybe Expr
car' (List (x:_))  = Just x
car' (Lit (LString xs)) = Just $ Lit. LChar $ head xs
car' (List [])     = Nothing

cdr :: Expr -> Expr
cdr (List [])          = error "empty list"
cdr (List [_])         = List []
cdr (List (_:xs))      = List xs
cdr (Lit (LString xs)) = Lit . LString $ tail xs

append' :: Expr -> Expr -> Expr
append' (List xs) (List ys)                   = List $ xs ++ ys
append' (Lit (LString xs)) (Lit (LString ys)) = Lit $ LString $ xs ++ ys

cons :: Expr -> Expr -> Expr
cons x1 (List []) = List [x1]
cons x1 (List [x2]) = List [x1, x2]
cons x1 (List xs) = List $ x1 : xs
cons _ _ = error ""

compose :: Expr -> Expr -> Expr
compose a b = Lit $ LString $ show a ++ " " ++ show b

------------------------------------
-- STRING MANIPULATION PRIMITIVES --
------------------------------------

ord' :: Expr -> Expr
ord' (Lit (LChar a)) = Lit . LInt . toInteger . DC.ord $ a
ord' _               = error ""

chr' :: Expr -> Expr
chr' (Lit (LInt n)) = Lit . LChar $ DC.chr $ fromIntegral n
chr' _              = error ""
