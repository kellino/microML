module Language.Typing.Env where

import Language.Syntax
import Language.Typing.Type

import qualified Data.Map as Map
import Data.Foldable (foldl')

data Env = TypeEnv { types :: Map.Map Name Scheme}

------------------------
-- TYPING ENVIRONMENT --
------------------------

-- auxiliary functions for manipulating the type environment, largely derived from
-- write you a haskell

empty :: Env
empty = TypeEnv Map.empty

extend :: Env -> (Name, Scheme) -> Env
extend env (x, s) = env { types = Map.insert x s (types env) }

delete :: Env -> Name -> Env
delete (TypeEnv env) x = TypeEnv (Map.delete x env)

lookup :: Name -> Env -> Maybe Scheme
lookup k (TypeEnv env) = Map.lookup k env

merge :: Env -> Env -> Env
merge (TypeEnv a) (TypeEnv b) = TypeEnv (Map.union a b)

mergeEnvs :: [Env] -> Env
mergeEnvs = foldl' merge empty

listKeys :: Env -> [Name]
listKeys (TypeEnv env) = Map.keys env

fromList :: [(Name, Scheme)] -> Env
fromList xs = TypeEnv (Map.fromList xs)

toList :: Env -> [(Name, Scheme)]
toList (TypeEnv env) = Map.toList env

instance Monoid Env where
    mempty  = empty
    mappend = merge
