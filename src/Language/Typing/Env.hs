module Language.Typing.Env where

import Language.Syntax
import Language.Typing.Type

import qualified Data.Map as Map

data Env = TypeEnv { types :: Map.Map Name TypeScheme }

extend :: Env -> (Name , TypeScheme) -> Env
extend env (var, ts) =  env { types = Map.insert var ts (types env) }

restrict :: Env -> Name -> Env
restrict (TypeEnv env) n = TypeEnv $ Map.delete n env
