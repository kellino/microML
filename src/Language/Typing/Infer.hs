{-# LANGUAGE FlexibleInstances #-}

module Language.Typing.Infer (
  Constraint,
  TypeError(..),
  Subst(..),
  inferTop,
  constraintsExpr
) where

import Language.Typing.Env
import Language.Typing.Substitutable
import Language.Typing.TypeError
import qualified Language.Typing.Env as Env
import Language.Typing.Type
import Language.Syntax

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.RWS
import Control.Monad.Identity

import Data.List (nub)
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
    as' <- mapM $ const fresh
    let s = Subst $ Map.fromList $ zip as as'
    return $ apply s t

generalize :: Env -> Type -> TypeScheme
generalize env t  = Forall as t
    where as = Set.toList $ ftv t `Set.difference` ftv env

ops :: Map.Map Binop Type
ops = Map.fromList [
      (OpAdd, typeInt `TArr` (typeInt `TArr` typeInt))
    , (OpMul, typeInt `TArr` (typeInt `TArr` typeInt))
    , (OpSub, typeInt `TArr` (typeInt `TArr` typeInt))
    , (OpEq, typeInt `TArr` (typeInt `TArr` typeBool))
  ]

infer :: Expr -> Infer Type
infer expr = case expr of
  Lit (LInt _)  -> return typeInt
  Lit (LBoolean _) -> return typeBool

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
    tv <- fresh
    let u1 = t1 `TArr` (t2 `TArr` tv)
        u2 = ops Map.! op
    uni u1 u2
    return tv

  If cond tr fl -> do
    t1 <- infer cond
    t2 <- infer tr
    t3 <- infer fl
    uni t1 typeBool
    uni t2 t3
    return t2

inferTop :: Env -> [(String, Expr)] -> Either TypeError Env
inferTop env [] = Right env
inferTop env ((name, ex):xs) = case (inferExpr env ex) of
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
      solver (su1 `compose` su, (apply su1 cs0))

bind ::  TVar -> Type -> Solve Subst
bind a t | t == TVar a     = return emptySubst
         | occursCheck a t = throwError $ InfiniteType a t
         | otherwise       = return $ (Subst $ Map.singleton a t)

occursCheck ::  Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t
