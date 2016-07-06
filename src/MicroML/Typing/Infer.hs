{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module MicroML.Typing.Infer (
  inferTop,
  constraintsExpr,
) where

import MicroML.Typing.Env
import MicroML.Typing.Substitutable
import MicroML.Typing.TypeError
import qualified MicroML.Typing.Env as Env
import MicroML.Typing.Type
import MicroML.Syntax

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.RWS
import Control.Monad.Identity

import Data.List (nub)
-- import Data.Char (ord)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Inference monad
type Infer a = (RWST Env [Constraint] InferState (Except TypeError) a)  

-- | Inference state
data InferState = InferState { count :: Int }

-- | Initial inference state
initInfer :: InferState
initInfer = InferState { count = 0 }

-------------------------------------------------------------------------------
-- Inference
-------------------------------------------------------------------------------

-- | Run the inference monad
runInfer :: Env -> Infer Type -> Either TypeError (Type, [Constraint])
runInfer env m = runExcept $ evalRWST m env initInfer

-- | Solve for the toplevel type of an expression in a given environment
inferExpr :: Env -> Expr -> Either TypeError TypeScheme
inferExpr env ex = case runInfer env (infer ex) of
  Left err -> Left err
  Right (ty, cs) -> case runSolve cs of
    Left err -> Left err
    Right subst -> Right $ closeOver $ apply subst ty

-- | Return the internal constraints used in solving for the type of an expression
constraintsExpr :: Env -> Expr -> Either TypeError ([Constraint], Subst, Type, TypeScheme)
constraintsExpr env ex = case runInfer env (infer ex) of
  Left err -> Left err
  Right (ty, cs) -> case runSolve cs of
    Left err -> Left err
    Right subst -> Right (cs, subst, ty, sc)
      where
        sc = closeOver $ apply subst ty

-- | Canonicalize and return the polymorphic toplevel type.
closeOver :: Type -> TypeScheme
closeOver = normalize . generalize Env.empty

-- | Unify two types
uni :: Type -> Type -> Infer ()
uni t1 t2 = tell [(t1, t2)]

-- | Extend type environment
inEnv :: (Name, TypeScheme) -> Infer a -> Infer a
inEnv (x, sc) m = do
  let scope e = restrict e x `extend` (x, sc)
  local scope m

-- | Lookup type in the environment
lookupEnv :: Name -> Infer Type
lookupEnv x = do
  (TypeEnv env) <- ask
  case Map.lookup x env of
      Nothing   ->  throwError $ UnboundVariable x
      Just s    ->  instantiate s

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: Infer Type
fresh = do
    s <- get
    put s{count = count s + 1}
    return $ TVar $ TV (letters !! count s)

instantiate ::  TypeScheme -> Infer Type
instantiate (Forall as t) = do
   -- as' <- mapM (\_ -> fresh) as
    as' <- mapM (const fresh) as
    let s = Subst $ Map.fromList $ zip as as'
    return $ apply s t

generalize :: Env -> Type -> TypeScheme
generalize env t  = Forall as t
    where as = Set.toList $ ftv t `Set.difference` ftv env

mathsOps :: Map.Map Binop Type
mathsOps = Map.fromList [
       ( OpAdd, typeNum `TArr`   ( typeNum `TArr` typeNum))
    ,  ( OpMul, typeNum `TArr`   ( typeNum `TArr` typeNum))
    ,  ( OpSub, typeNum `TArr`   ( typeNum `TArr` typeNum))
    ,  ( OpMod, typeNum `TArr`   ( typeNum `TArr` typeNum))
    ,  ( OpExp, typeNum `TArr`   ( typeNum `TArr` typeNum))
    ,  ( OpGe, typeNum `TArr`    ( typeNum `TArr` typeBool))
    ,  ( OpLe, typeNum `TArr`    ( typeNum `TArr` typeBool))
    ,  ( OpGt, typeNum `TArr`    ( typeNum `TArr` typeBool))
    ,  ( OpLt, typeNum `TArr`    ( typeNum `TArr` typeBool))
    ,  ( OpEq, typeNum `TArr`    ( typeNum `TArr` typeBool))
    ,  ( OpNotEq, typeNum `TArr` ( typeNum `TArr` typeBool))
  ]

infer :: Expr -> Infer Type
infer expr = case expr of
    Lit (LInt _)     -> return typeNum
    Lit (LDouble _)  -> return typeNum
    Lit (LBoolean _) -> return typeBool
    Lit (LString _)  -> return typeString
    Lit (LChar _)    -> return typeChar

    List []   -> return typeList
    List [x]  -> do
        t1 <- infer x
        tv <- fresh
        uni t1 tv
        return t1
    List (x:y:xs) -> do
        t1 <- infer x
        t2 <- infer y
        uni t1 t2
        infer $ List (y:xs)

    Var x -> lookupEnv x

    Lam x e -> do
        tv <- fresh
        t <- inEnv (x, Forall [] tv) (infer e)
        return (tv `TArr` t)

    App e1 e2 -> do
        t1 <- infer e1
        t2 <- infer e2
        tv <- fresh
        uni t1 (t2 `TArr` tv)
        return tv

    Let x e1 e2 -> do
        env <- ask
        t1 <- infer e1
        let sc = generalize env t1
        t2 <- inEnv (x, sc) (infer e2)
        return t2

    FixPoint e1 -> do
        t1 <- infer e1
        tv <- fresh
        uni (tv `TArr` tv) t1
        return tv

    Op op e1 e2 -> do
        t1 <- infer e1
        t2 <- infer e2
        if (t1 == typeNum) && (t2 == typeNum)
           then doOp op t1 t2
           else return t1

    If cond tr fl -> do
        t1 <- infer cond
        t2 <- infer tr
        t3 <- infer fl
        uni t1 typeBool
        uni t2 t3
        return t2

getOp :: (Ord k) => Map.Map k Type -> k -> Type -> Type -> Infer Type
getOp dict op t1 t2 = do
    tv <- fresh
    let u1 = t1 `TArr` (t2 `TArr` tv)
        u2 = dict Map.! op
    uni u1 u2
    return tv

doOp :: Binop -> Type -> Type -> Infer Type
doOp op t1 t2= 
    case op of 
      OpAdd   -> getOp mathsOps OpAdd t1 t2
      OpSub   -> getOp mathsOps OpSub t1 t2
      OpMul   -> getOp mathsOps OpMul t1 t2
      OpDiv   -> getOp mathsOps OpDiv t1 t2
      OpMod   -> getOp mathsOps OpMod t1 t2
      OpExp   -> getOp mathsOps OpExp t1 t2
      OpEq    -> getOp mathsOps OpEq t1 t2
      OpLe    -> getOp mathsOps OpLe t1 t2
      OpLt    -> getOp mathsOps OpLt t1 t2
      OpGe    -> getOp mathsOps OpGe t1 t2
      OpGt    -> getOp mathsOps OpGt t1 t2
      OpNotEq -> getOp mathsOps OpNotEq t1 t2

inferTop :: Env -> [(String, Expr)] -> Either TypeError Env
inferTop env [] = Right env
inferTop env ((name, ex):xs) = case inferExpr env ex of
  Left err -> Left err
  Right ty -> inferTop (extend env (name, ty)) xs

normalize :: TypeScheme -> TypeScheme
normalize (Forall _ body) = Forall (map snd ord) (normtype body)
  where
    ord = zip (nub $ fv body) (map TV letters)

    fv (TVar a)   = [a]
    fv (TArr a b) = fv a ++ fv b
    fv (TCon _)    = []

    normtype (TArr a b) = TArr (normtype a) (normtype b)
    normtype (TCon a)   = TCon a
    normtype (TVar a)   =
      case Prelude.lookup a ord of
        Just x -> TVar x
        Nothing -> error "type variable not in signature"

-------------------------------------------------------------------------------
-- Constraint Solver
-------------------------------------------------------------------------------

-- | The empty substitution
emptySubst :: Subst
emptySubst = mempty

-- | Compose substitutions
compose :: Subst -> Subst -> Subst
(Subst s1) `compose` (Subst s2) = Subst $ Map.map (apply (Subst s1)) s2 `Map.union` s1

-- | Run the constraint solver
runSolve :: [Constraint] -> Either TypeError Subst
runSolve cs = runIdentity $ runExceptT $ solver st
  where st = (emptySubst, cs)

unifyMany :: [Type] -> [Type] -> Solve Subst
unifyMany [] [] = return emptySubst
unifyMany (t1 : ts1) (t2 : ts2) =
  do su1 <- unifies t1 t2
     su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
     return (su2 `compose` su1)
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

unifies :: Type -> Type -> Solve Subst
unifies t1 t2 | t1 == t2 = return emptySubst
unifies (TVar v) t = v `bind` t
unifies t (TVar v) = v `bind` t
unifies (TArr t1 t2) (TArr t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies t1 t2 = throwError $ UnificationFail t1 t2

-- Unification solver
solver :: Unifier -> Solve Subst
solver (su, cs) =
  case cs of
    [] -> return su
    ((t1, t2): cs0) -> do
      su1  <- unifies t1 t2
      solver (su1 `compose` su, apply su1 cs0)

bind ::  TVar -> Type -> Solve Subst
bind a t | t == TVar a     = return emptySubst
         | occursCheck a t = throwError $ InfiniteType a t
         | otherwise       = return (Subst $ Map.singleton a t)

occursCheck ::  Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t
