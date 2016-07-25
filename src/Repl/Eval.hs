module Repl.Eval where

import MicroML.Syntax
import MicroML.ListPrimitives

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Bits (xor)

emptyTmenv :: TermEnv
emptyTmenv = Map.empty

eval :: TermEnv -> Expr -> Expr
eval env expr = case expr of
    num@(Lit (LInt _))      -> num
    doub@(Lit (LDouble _))  -> doub
    char@(Lit (LChar _))    -> char
    str@(Lit (LString _))   -> str
    bool@(Lit (LBoolean _)) -> bool
    tup@(Lit (LTup _))      -> tup
    Nil                     -> Nil
    Var x                   -> fromJust (Map.lookup x env) -- the type checker ensures we never get this far
    FixPoint e              -> eval env (App e (FixPoint e))
    Lam x body              -> Closure x body env
    App a b                 -> do
        let Closure n expr' clo = eval env a
        let arg = eval env b
        let new' = Map.insert n arg clo
        eval new' expr'
    If cond tr fls          -> do
        let cond' = eval env cond
        if cond' == Lit (LBoolean True)
           then eval env tr
           else eval env fls
    Let x e body            -> do
        let e' = eval env e
        let new' = Map.insert x e' env
        eval new' body
    UnaryOp op a -> do
        let a' = eval env a
        case op of
          Car   -> car a'
          Cdr   -> cdr a'

          OpLog -> log' a'
          Minus ->  case a' of
                      (Lit (LInt x))    -> Lit . LInt $ negate x
                      (Lit (LDouble x)) -> Lit . LDouble $ negate x
          Not   -> case a' of
                   (Lit (LBoolean True)) -> Lit . LBoolean $ False
                   (Lit (LBoolean False)) -> Lit . LBoolean $ True
    Op op a b -> do
        let a' = eval env a
        let b' = eval env b
        case op of
          OpAdd    -> a' `add` b'
          OpSub    -> a' `sub`  b'
          OpMul    -> a' `mul`  b'
          OpDiv    -> a' `div'` b'
          OpIntDiv -> a' `intDiv` b'
          OpMod    -> a' `mod'` b'
          OpExp    -> a' `exp'` b'
          OpOr     -> a' `or'`  b'
          OpAnd    -> a' `and'` b'
          OpXor    -> a' `xor'` b'
          OpEq     -> a' `opEq` b'
          OpLe     -> a' `opLe` b'
          OpLt     -> a' `opLt` b'
          OpGe     -> a' `opGe` b'
          OpGt     -> a' `opGt` b'
          OpNotEq  -> a' `opNotEq` b'
          OpCons   -> a' `cons` b'

cons :: Expr -> Expr -> Expr
cons a Nil = Op OpCons a Nil
cons a (Op OpCons x Nil) = Op OpCons a (Op OpCons x Nil)
cons a ls@(Op OpCons _ _) = Op OpCons a ls

add :: Expr -> Expr -> Expr
add (Lit (LInt a)) (Lit (LInt b)) = Lit $ LInt $ a + b
add (Lit (LDouble a)) (Lit (LDouble b)) = Lit $ LDouble $ a + b
add (Lit (LInt a)) (Lit (LDouble b)) = Lit $ LDouble $ realToFrac a + b
add (Lit (LDouble a)) (Lit (LInt b)) = Lit $ LDouble $ a + realToFrac b

or', and', xor' :: Expr -> Expr -> Expr
or' (Lit (LBoolean a)) (Lit (LBoolean b)) = Lit $ LBoolean $ a || b
and' (Lit (LBoolean a)) (Lit (LBoolean b)) = Lit $ LBoolean $ a && b
xor' (Lit (LBoolean a)) (Lit (LBoolean b)) = Lit $ LBoolean $ a `xor` b

sub :: Expr -> Expr -> Expr
sub (Lit (LInt a)) (Lit (LInt b)) = Lit $ LInt $ a - b
sub (Lit (LDouble a)) (Lit (LDouble b)) = Lit $ LDouble $ a - b
sub (Lit (LInt a)) (Lit (LDouble b)) = Lit $ LDouble $ realToFrac a - b
sub (Lit (LDouble a)) (Lit (LInt b)) = Lit $ LDouble $ a - realToFrac b

mul :: Expr -> Expr -> Expr
mul (Lit (LInt a)) (Lit (LInt b)) = Lit $ LInt $ a * b
mul (Lit (LDouble a)) (Lit (LDouble b)) = Lit $ LDouble $ a * b
mul (Lit (LInt a)) (Lit (LDouble b)) = Lit $ LDouble $ realToFrac a * b
mul (Lit (LDouble a)) (Lit (LInt b)) = Lit $ LDouble $ a * realToFrac b

div' :: Expr -> Expr -> Expr
div' (Lit (LInt a)) (Lit (LInt b)) = Lit $ LDouble $ realToFrac a / realToFrac b
div' (Lit (LDouble a)) (Lit (LDouble b)) = Lit $ LDouble $ a / b
div' (Lit (LInt a)) (Lit (LDouble b)) = Lit $ LDouble $ realToFrac a / b
div' (Lit (LDouble a)) (Lit (LInt b)) = Lit $ LDouble $ a / realToFrac b

intDiv :: Expr -> Expr -> Expr
intDiv (Lit (LInt a)) (Lit (LInt b)) = Lit . LInt $ a `div` b
intDiv (Lit (LDouble a)) (Lit (LDouble b)) = Lit $ LInt $ floor a `div` floor b
intDiv (Lit (LInt a)) (Lit (LDouble b)) = Lit $ LInt $ a `div` floor b
intDiv (Lit (LDouble a)) (Lit (LInt b)) = Lit $ LInt $ floor a `div` b

mod' :: Expr -> Expr -> Expr
mod' (Lit (LInt a)) (Lit (LInt b)) = Lit $ LInt $ a `mod` b
mod' _ _ = error "only works on integers" -- improve this 

exp' :: Expr -> Expr -> Expr
exp' (Lit (LInt a)) (Lit (LInt b)) = Lit $ LInt $ a^b
exp' (Lit (LInt a)) (Lit (LDouble b)) = Lit $ LDouble $ realToFrac a**b
exp' (Lit (LDouble a)) (Lit (LInt b)) = Lit $ LDouble $ a ^ b
exp' (Lit (LDouble a)) (Lit (LDouble b)) = Lit $ LDouble $ a**b

log' :: Expr -> Expr 
log' (Lit (LInt a))    = Lit . LDouble $ log $ realToFrac a
log' (Lit (LDouble a)) = Lit . LDouble $ log a

opEq :: Expr -> Expr -> Expr
opEq (Lit (LInt a)) (Lit (LDouble b)) = Lit . LBoolean $ realToFrac a == b
opEq (Lit (LDouble a)) (Lit (LInt b)) = Lit . LBoolean $ a == realToFrac b
opEq a b = Lit . LBoolean $ a == b

opLe :: Expr -> Expr -> Expr
opLe (Lit (LInt a)) (Lit (LDouble b)) = Lit . LBoolean $ realToFrac a <= b
opLe (Lit (LDouble a)) (Lit (LInt b)) = Lit . LBoolean $ a <= realToFrac b
opLe a b = Lit . LBoolean $ a <= b

opLt :: Expr -> Expr -> Expr
opLt (Lit (LInt a)) (Lit (LDouble b)) = Lit . LBoolean $ realToFrac a < b
opLt (Lit (LDouble a)) (Lit (LInt b)) = Lit . LBoolean $ a < realToFrac b
opLt a b = Lit . LBoolean $ a < b

opGt :: Expr -> Expr -> Expr
opGt (Lit (LInt a)) (Lit (LDouble b)) = Lit . LBoolean $ realToFrac a > b
opGt (Lit (LDouble a)) (Lit (LInt b)) = Lit . LBoolean $ a > realToFrac b
opGt a b = Lit . LBoolean $ a > b

opGe :: Expr -> Expr -> Expr
opGe (Lit (LInt a)) (Lit (LDouble b)) = Lit . LBoolean $ realToFrac a >= b
opGe (Lit (LDouble a)) (Lit (LInt b)) = Lit . LBoolean $ a >= realToFrac b
opGe a b = Lit . LBoolean $ a >= b

opNotEq :: Expr -> Expr -> Expr
opNotEq (Lit (LInt a)) (Lit (LDouble b)) = Lit . LBoolean $ realToFrac a /= b
opNotEq (Lit (LDouble a)) (Lit (LInt b)) = Lit . LBoolean $ a /= realToFrac b
opNotEq a b = Lit . LBoolean $ a /= b

runEval :: TermEnv -> String -> Expr -> (Expr, TermEnv)
runEval env x exp = 
    let res = eval env exp
     in (res, Map.insert x res env)
