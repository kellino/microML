module MicroML.Typing.Env where

import MicroML.Syntax
import MicroML.Typing.Type

import qualified Data.Map as Map

data Env = TypeEnv { types :: Map.Map Name TypeScheme }
    deriving (Eq, Show)

empty :: Env
empty = TypeEnv Map.empty

-- unfortunately the type checker can't expose the types of built-in functions if 
-- requested in the repl (using the :typeof of :browse commands
-- a simple fix is to add them in manually here, but it's a little bit of a kludge
polyA :: TVar
polyA = TV "a"

microbit :: Env
microbit = TypeEnv $ Map.fromList
    [ ("scroll" , Forall [polyA] $ TArrow (TVar polyA) (TVar polyA))
    , ("head"   , Forall [TV "[a]"] $ TArrow (TVar $ TV "[a]") (TVar polyA))
    , ("tail"   , Forall [TV "[a]"] $ TArrow (TVar $ TV "[a]") (TVar $ TV "[a]"))
    , (":"      , Forall [TV "a", TV "[a]"] $ (TVar $ TV "a") `TArrow` (TVar $ TV "[a]") `TArrow` (TVar $ TV "[a]"))
    , ("+"      , Forall  [] $ TArrow typeNum (TArrow typeNum typeNum))
    , ("-"      , Forall  [] $ TArrow typeNum (TArrow typeNum typeNum))
    , ("/"      , Forall  [] $ TArrow typeNum (TArrow typeNum typeNum))
    , ("//"     , Forall  [] $ TArrow typeNum (TArrow typeNum typeNum))
    , ("%"      , Forall  [] $ TArrow typeNum (TArrow typeNum typeNum))
    , ("^"      , Forall  [] $ TArrow typeNum (TArrow typeNum typeNum))
    , ("=="     , Forall  [] $ TArrow (TVar polyA) (TArrow (TVar polyA) typeBool))
    , (">>"     , Forall  [polyA, TV "b"] $ TVar polyA `TArrow` (TVar polyA `TArrow` TVar (TV "b") `TArrow` (TVar $ TV "b")))
    , ("show"   , Forall  [polyA] $ TVar polyA `TArrow` typeString)
    , ("read"   , Forall  [] $ typeString `TArrow` typeNum)
    ]

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
