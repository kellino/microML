module Repl.Eval where

import MicroML.Syntax
import MicroML.ListPrimitives
import MicroML.MathsPrimitives

import qualified Data.Map as Map
import Data.Maybe (fromJust)

import Control.Monad.Except
import Control.Monad.Identity

emptyTmenv :: TermEnv
emptyTmenv = Map.empty

hoistPrimError :: Show a => Either a Expr -> Expr
hoistPrimError e = 
    case e of
      Right val -> val
      Left err  -> PrimitiveErr $ ListPrim $ show err

eval :: TermEnv -> Expr -> Eval Expr
eval env expr = case expr of
    num@(Lit (LInt _))      -> return num
    doub@(Lit (LDouble _))  -> return doub
    char@(Lit (LChar _))    -> return char
    str@(Lit (LString _))   -> return str
    bool@(Lit (LBoolean _)) -> return bool
    tup@(Lit (LTup _))      -> return tup
    Nil                     -> return Nil
    Var x                   -> return $ fromJust (Map.lookup x env) -- the type checker ensures we never get this far
    FixPoint e              -> eval env (App e (FixPoint e))
    Lam x body              -> return $ Closure x body env
    App a b                 -> do
        Closure n expr' clo <- eval env a
        arg <- eval env b
        let new' = Map.insert n arg clo
        eval new' expr'
    If cond tr fls          -> do
        cond' <- eval env cond
        if cond' == Lit (LBoolean True)
           then eval env tr
           else eval env fls
    Let x e body            -> do
        e' <- eval env e
        let new' = Map.insert x e' env
        eval new' body
    UnaryOp op a -> do
        a' <- eval env a
        case op of
          Show  -> show' a'
          {-Read  -> read' a'-}
          {-Car   -> car a'-}
          {-Cdr   -> cdr a'-}
          {-OpLog -> log' a'-}
          {-Chr   -> chr' a'-}
          {-Ord   -> ord' a'-}
          Minus ->  case a' of
                      (Lit (LInt x))    -> Lit . LInt $ negate x
                      (Lit (LDouble x)) -> Lit . LDouble $ negate x
          {-Not   -> case a' of-}
                   {-(Lit (LBoolean True)) -> Lit . LBoolean $ False-}
                   {-(Lit (LBoolean False)) -> Lit . LBoolean $ True-}
    BinOp op a b -> do
        a' <- eval env a
        b' <- eval env b
        case op of
          {-OpEnum   -> enumFromTo_ a' b'-}
          {-OpAdd    -> a' `add` b'-}
          OpSub    -> a' `sub`  b'
          {-OpMul    -> a' `mul`  b'-}
          {-OpDiv    -> a' `div'` b'-}
          {-OpIntDiv -> a' `intDiv` b'-}
          {-OpMod    -> a' `mod'` b'-}
          {-OpExp    -> a' `exp'` b'-}
          {-OpOr     -> a' `or'`  b'-}
          {-OpAnd    -> a' `and'` b'-}
          {-OpXor    -> a' `xor'` b'-}
          {-OpEq     -> a' `opEq` b'-}
          {-OpLe     -> a' `opLe` b'-}
          {-OpLt     -> a' `opLt` b'-}
          {-OpGe     -> a' `opGe` b'-}
          {-OpGt     -> a' `opGt` b'-}
          {-OpNotEq  -> a' `opNotEq` b'-}
          {-OpCons   -> a' `cons` b'-}
          {-OpAppend -> eval env (a' `append` b')-}
          {-OpPipe   -> eval env (App b a)-}

runEval :: TermEnv -> String -> Expr -> Either String (Expr, TermEnv)
runEval env x exp = do
    let res = runIdentity $ runExceptT $ eval env exp
    case res of
         Left err -> Left err
         Right r -> Right (r, Map.insert x r env)
