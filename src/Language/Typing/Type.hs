module Language.Typing.Type where

newtype TVar = TV String
    deriving (Show, Eq, Ord)

data Type
    = TVar TVar
    | TCon String
    | TArr Type Type
      deriving (Show, Eq, Ord)

data Scheme = Forall [TVar] Type
    deriving (Show, Eq, Ord)

typeNum, typeDouble, typeBool :: Type
typeNum = TCon "Num"
typeDouble = TCon "Double"
typeBool = TCon "Boolean"
