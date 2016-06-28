module Language.Typing.Substitutable where

import Language.Typing.Type
import Language.Typing.Env

import Control.Monad (replicateM)
import qualified Data.Map as Map
import qualified Data.Set as Set


type Subst = Map.Map TVar Type


class Substitutable a where
    apply :: Subst -> a -> a
    ftv :: a -> Set.Set TVar

instance Substitutable Type where
    apply _ (TCon a) = TCon a
    apply s t@(TVar a) = Map.findWithDefault t a s
    apply s (t1 `TArr` t2) = apply s t1 `TArr` apply s t2

    ftv TCon{} = Set.empty              -- ftv(Primitive) = \0
    ftv (TVar a) = Set.singleton a      -- ftv(a) = {a}
    ftv (t1 `TArr` t2) = ftv t1 `Set.union` ftv t2  -- ftv(t1 -> t2) = ftv(t1) ∪ ftv(t2)

instance Substitutable TypeScheme where
    apply s (Forall as t) = Forall as $ apply s' t
        where s' = foldr Map.delete s as            
    ftv (Forall as t) = ftv t `Set.difference` Set.fromList as -- ftv(∀x.t) = ftv(t) - {x}

instance Substitutable a => Substitutable [a] where
    apply = fmap . apply
    ftv   = foldr (Set.union . ftv) Set.empty

instance Substitutable Env where
    apply s (TypeEnv env) = TypeEnv $ Map.map (apply s) env
    ftv (TypeEnv env) = ftv $ Map.elems env

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

