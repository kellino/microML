module Language.Typing.Type  where

newtype TVar = TV String
    deriving (Show, Eq, Ord)

data Type 
    = TVar TVar
    | TCon String
    | TArr Type Type
    deriving (Show, Eq, Ord)

data TypeScheme = Forall [TVar] Type
    deriving (Show, Eq, Ord)

typeNum    = TCon "Number"
typeBool   = TCon "Boolean"
typeString = TCon "String"
typeChar   = TCon "Char"
typeList   = TCon "List"
