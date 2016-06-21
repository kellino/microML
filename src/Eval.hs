module Eval where

import Syntax

import Control.Monad.Identity
import qualified Data.Map as Map

data Value 
    = VNum Integer
  | VBool Bool
  | VDouble Double
  | VClosure String Expr TermEnv
  deriving (Eq, Ord)

type TermEnv = Map.Map String Value
type Interpreter t = Identity t

emptyTmenv :: TermEnv
emptyTmenv = Map.empty

instance Show Value where
    show (VNum n)    = show n
    show (VBool b)   = show b
    show (VDouble d) = show d
    show VClosure{}  = "<<closure>>"

eval :: TermEnv -> Expr -> Interpreter Value
eval env expr = case expr of
    Lit (Number k) -> return $ VNum k
    Lit (Double k) -> return $ VDouble k
    Lam x body     -> return (VClosure x body env)
    Var x          -> do
        let Just v = Map.lookup x env
        return v
    If cond tr fl  -> do
        VBool br <- eval env cond
        if br
           then eval env tr
           else eval env fl
    Op op a b  -> do
        a' <- eval env a
        b' <- eval env b
        case op of
          OpAdd -> a' `add` b'
          OpSub -> a' `sub`  b'
          OpMul -> a' `mul`  b'
          OpDiv -> a' `div'` b'
          OpMod -> a' `mod'` b'
          OpExp -> a' `exp'` b'
          OpOr  -> a' `or'`  b'
          OpAnd -> a' `and'` b'
          OpEq  -> return $ VBool $ a' == b'
          OpLe  -> return $ VBool $ a' <= b'
          OpLt  -> return $ VBool $ a' <  b'
          OpGe  -> return $ VBool $ a' >= b'
          OpGt  -> return $ VBool $ a' >  b'
    _     -> error "not yet supported"

-- helper functions --

and' = undefined
or' = undefined
--or' (Lit (Boolean a)) (Lit (Boolean b)) = return $ VBool $ a || b

--and' (Boolean a) (Boolean b) = Boolean $ a && b

add (VNum a) (VNum b) = return $ VNum $ a + b
add (VDouble a) (VDouble b) = return $ VDouble $ a + b
add (VNum a) (VDouble b) = return $  VDouble $ realToFrac a + b
add (VDouble a) (VNum b) = return $ VDouble $ a + realToFrac b

sub (VNum a) (VNum b) = return $ VNum $ a - b
sub (VDouble a) (VDouble b) = return $ VDouble $ a - b
sub (VNum a) (VDouble b) = return $ VDouble $ realToFrac a - b
sub (VDouble a) (VNum b) = return $ VDouble $ a - realToFrac b

mul (VDouble a) (VDouble b) = return $ VDouble $ a * b
mul (VNum a) (VDouble b) = return $ VDouble $ realToFrac a * b
mul (VDouble a) (VNum b) = return $ VDouble $ a * realToFrac b

div' (VNum a) (VNum b) = return $ VDouble $ realToFrac a / realToFrac b
div' (VDouble a) (VDouble b) = return $ VDouble $ a / b
div' (VNum a) (VDouble b) = return $ VDouble $ realToFrac a / b
div' (VDouble a) (VNum b) = return $ VDouble $ a / realToFrac b

mod' (VNum a) (VNum b) = return $ VNum $ a `mod` b

exp' (VNum a) (VNum b) = return $ VNum $ a^b
exp' (VNum a) (VDouble b) = return $ VDouble $ realToFrac a**b
exp' (VDouble a) (VNum b) = return $ VDouble $ a ^ b
exp' (VDouble a) (VDouble b) = return $ VDouble $ a**b

runEval :: TermEnv -> String -> Expr -> (Value, TermEnv)
runEval env x exp =
    let res = runIdentity (eval env exp)
     in (res, Map.insert x res env)
