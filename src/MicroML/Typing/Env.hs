module Language.Typing.Env where

import Data.Monoid
import Language.Syntax
import Language.Typing.Type

import qualified Data.Map as Map

data Env = TypeEnv { types :: Map.Map Name TypeScheme }
    deriving (Eq, Show)

empty :: Env
empty = TypeEnv Map.empty

lookup :: Name -> Env -> Maybe TypeScheme
lookup k (TypeEnv env) = Map.lookup k env

extend :: Env -> (Name , TypeScheme) -> Env
extend env (var, ts) =  env { types = Map.insert var ts (types env) }

restrict :: Env -> Name -> Env
restrict (TypeEnv env) n = TypeEnv $ Map.delete n env

merge :: Env -> Env -> Env
merge (TypeEnv a) (TypeEnv b) = TypeEnv (Map.union a b)

instance Monoid Env where
    mempty = empty
    mappend = merge
