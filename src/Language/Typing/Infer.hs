{-# LANGUAGE FlexibleContexts #-}

module Language.Typing.Infer  where

import Language.Syntax
import Language.Typing.Substitutable
import Language.Typing.Env
import Language.Typing.Type
import Language.Typing.TypeError

import Control.Monad.Trans.RWS
import Control.Monad.Except
import qualified Data.Map as Map
import qualified Data.Set as Set

type Infer a = (RWST Env [Constraint] InferState (Except TypeError) a)

type Constraint = (Type, Type)
data InferState = InferState { count :: Int }

unifier :: Type -> Type -> Infer ()
unifier t1 t2 = tell [(t1, t2)]

inEnv :: (Name, TypeScheme) -> Infer a -> Infer a
inEnv (x, sc) m = do
    let scope e = restrict e x `extend` (x, sc)
    local scope m

freshName :: Infer Type
freshName = do
    s <- get
    put s{count = count s + 1}
    return $ TVar $ TV (letters !! count s)

lookupInEnv :: Name -> Infer Type
lookupInEnv x = do
    TypeEnv env <- ask
    case Map.lookup x env of
      Nothing -> throwError $ UnboundVariable x
      Just k  -> instantiate k

instantiate :: TypeScheme -> Infer Type
instantiate (Forall as t) = do
    as' <- mapM (const freshName) as
    let s = Map.fromList $ zip as as'
    return $ apply s t

generalize :: Env -> Type -> TypeScheme
generalize env t = Forall as t
    where as = Set.toList $ ftv t `Set.difference` ftv env

nullSubst :: Subst
nullSubst = Map.empty

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = Map.map (apply s1) s2 `Map.union` s1

infer :: Expr -> Infer Type
infer expr = case expr of
    (Lit (LInt _))     -> return typeInt
    (Lit (LDouble _))  -> return typeDouble
    (Lit (LBoolean _)) -> return typeBool
    (Lit (LString _))  -> return typeString
    (Lit (LChar _))    -> return typeChar

    Var x -> lookupInEnv x
