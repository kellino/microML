module Language.Typing.Type  where

import Unbound.LocallyNameless

newtype TVar = TV String
    deriving (Show, Eq, Ord)

data Type 
    = TVar (Name TVar)
    | TCon String
    | TArr Type Type
    deriving (Show, Eq, Ord)

data TypeScheme = Forall [TVar] Type
    deriving (Show, Eq, Ord)

typeInt, typeDouble, typeBool :: Type
typeInt    = TCon "Integer"
typeDouble = TCon "Double"
typeBool   = TCon "Boolean"
