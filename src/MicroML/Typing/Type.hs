{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module MicroML.Typing.Type  where

newtype TVar = TV String
    deriving (Show, Eq, Ord)

data Type 
    = TVar TVar
    | TCon String
    | TArrow Type Type
    deriving (Show, Eq, Ord)

data TypeScheme = Forall [TVar] Type
    deriving (Show, Eq, Ord)

typeNum       = TCon "Number"
typeBool      = TCon "Boolean"
typeString    = TCon "String"
typeChar      = TCon "Char"
typeTup       = TCon $ "{" ++ "Tuple" ++ "}"
typeError     = TCon "Error"
typeNil       = TVar $ TV "[a]"
