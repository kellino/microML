{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module MicroML.Typing.Type  where

newtype TVar = TV String
    deriving (Show, Eq, Ord)

data Type 
    = TVar TVar
    | TCon String
    | TArr Type Type
    deriving (Show, Eq, Ord)

data TypeScheme = Forall [TVar] Type
    deriving (Show, Eq, Ord)

typeNum       = TCon "\ESC[31mNumber\ESC[0m"
typeBool      = TCon "\ESC[32mBoolean\ESC[0m"
typeString    = TCon "\ESC[33mString\ESC[0m"
typeChar      = TCon "\ESC[34mChar\ESC[0m"
typeNil       = TCon "\ESC[35m[*]\ESC[0m"
typeTup       = TCon $ "{" ++ "\ESC[36mTuple\ESC[0m" ++ "}"
typeListofNumber = TCon "[\ESC[31mNumber\ESC[0m]"
typeListofBool = TCon "[\ESC[32mBoolean\ESC[0m]"
typeListofChar  = TCon "[\ESC[34mChar\ESC[0m]"
typeListofString  = TCon "[\ESC[33mString\ESC[0m]"
typeListofTup = TCon "[\ESC[36mTuple\ESC[0m]"
