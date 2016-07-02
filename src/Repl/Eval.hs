module Repl.Eval where

import MicroML.Syntax

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

emptyTmenv :: TermEnv
emptyTmenv = Map.empty

eval :: TermEnv -> Expr -> Expr
eval env expr = case expr of
    num@(Lit (LInt _))      -> num
    doub@(Lit (LDouble _))  -> doub
    char@(Lit (LChar _))    -> char
    str@(Lit (LString _))   -> str
    bool@(Lit (LBoolean _)) -> bool
    FixPoint e              -> eval env (App e (FixPoint e))
    List xs                 -> List $ map (eval env) xs
    Var x                   -> fromMaybe (MLError "not yet been set") (Map.lookup x env)
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
    Op op a b -> do
        let a' = eval env a
        let b' = eval env b
        case op of
          OpAdd -> a' `add` b'

add :: Expr -> Expr -> Expr
add (Lit (LInt a)) (Lit (LInt b)) = Lit $ LInt $ a + b
add (Lit (LDouble a)) (Lit (LDouble b)) = Lit $ LDouble $ a + b
add (Lit (LInt a)) (Lit (LDouble b)) = Lit $ LDouble $ realToFrac a + b
add (Lit (LDouble a)) (Lit (LInt b)) = Lit $ LDouble $ a + realToFrac b
add _ _ = error "weird"

{-eval :: TermEnv -> Expr -> Interpreter Value-}
{-eval env expr = case expr of-}
    {-List xs           -> return $ VList $ map (eval env) xs-}
    {-Let x e body      -> do-}
        {-e' <- eval env e-}
        {-let newEnv = Map.insert x e' env-}
        {-eval newEnv body-}
    {-Op op a b  -> do-}
        {-a' <- eval env a-}
        {-b' <- eval env b-}
        {-case op of-}
          {-OpAdd -> return $ a' `add` b'-}
          {-OpSub -> return $ a' `sub`  b'-}
          {-OpMul -> return $ a' `mul`  b'-}
          {-OpDiv -> return $ a' `div'` b'-}
          {-OpMod -> return $ a' `mod'` b'-}
          {-OpExp -> return $ a' `exp'` b'-}
          {-OpOr  -> return $ a' `or'`  b'-}
          {-OpAnd -> return $ a' `and'` b'-}
          {-OpEq  -> return $ VBool $ a' == b'-}
          {-OpLe  -> return $ VBool $ a' <= b'-}
          {-OpLt  -> return $ VBool $ a' <  b'-}
          {-OpGe  -> return $ VBool $ a' >= b'-}
          {-OpGt  -> return $ VBool $ a' >  b'-}
          {-OpNotEq -> return $ VBool $ a' /= b'-}
    {-UnaryMinus ex -> do-}
        {-ex' <- eval env ex-}
        {-case ex' of-}
          {-VNum n    -> return $ VNum (negate n)-}
          {-VDouble d -> return $ VDouble (negate d)-}
    {-_     -> return $ VError "not yet supported"-}

{--- helper functions ---}

{-or' (VBool a) (VBool b) = VBool $ a || b-}

{-and' (VBool a) (VBool b)  = VBool $ a && b-}

{-add :: Value -> Value -> Value-}
{-add (VNum a) (VNum b) = VNum $ a + b-}
{-add (VDouble a) (VDouble b) = VDouble $ a + b-}
{-add (VNum a) (VDouble b) = VDouble $ realToFrac a + b-}
{-add (VDouble a) (VNum b) = VDouble $ a + realToFrac b-}
{-add _ _                  = VError "please check your calculation..."-}

{-sub :: Value -> Value -> Value-}
{-sub (VNum a) (VNum b) = VNum $ a - b-}
{-sub (VDouble a) (VDouble b) = VDouble $ a - b-}
{-sub (VNum a) (VDouble b) = VDouble $ realToFrac a - b-}
{-sub (VDouble a) (VNum b) = VDouble $ a - realToFrac b-}
{-sub _ _                  = VError "please check your calculation..."-}

{-mul :: Value -> Value -> Value-}
{-mul (VNum a) (VNum b) = VNum $ a * b-}
{-mul (VDouble a) (VDouble b) = VDouble $ a * b-}
{-mul (VNum a) (VDouble b) = VDouble $ realToFrac a * b-}
{-mul (VDouble a) (VNum b) = VDouble $ a * realToFrac b-}
{-mul _ _                  = VError "please check your calculation..."-}

{-div' :: Value -> Value -> Value-}
{-div' (VNum a) (VNum b) = VDouble $ realToFrac a / realToFrac b-}
{-div' (VDouble a) (VDouble b) = VDouble $ a / b-}
{-div' (VNum a) (VDouble b) = VDouble $ realToFrac a / b-}
{-div' (VDouble a) (VNum b) = VDouble $ a / realToFrac b-}
{-div' _ _                  = VError "please check your calculation..."-}

{-mod' :: Value -> Value -> Value-}
{-mod' (VNum a) (VNum b) = VNum $ a `mod` b-}
{-mod' _ _                  = VError "please check your calculation..."-}

{-exp' :: Value -> Value -> Value-}
{-exp' (VNum a) (VNum b) = VNum $ a^b-}
{-exp' (VNum a) (VDouble b) = VDouble $ realToFrac a**b-}
{-exp' (VDouble a) (VNum b) = VDouble $ a ^ b-}
{-exp' (VDouble a) (VDouble b) = VDouble $ a**b-}
{-exp' _ _                  = VError "please check your calculation..."-}


runEval :: TermEnv -> String -> Expr -> (Expr, TermEnv)
runEval env x exp = 
    let res = eval env exp
     in (res, Map.insert x res env)

{-runEval :: TermEnv -> String -> Expr -> (Value, TermEnv)-}
{-runEval env x exp =-}
    {-let res = runIdentity (eval env exp)-}
     {-in (res, Map.insert x res env)-}
