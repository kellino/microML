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

typeNum :: Type
typeNum       = TCon "\ESC[31mNumber\ESC[0m"
typeBool      = TCon "\ESC[32mBoolean\ESC[0m"
typeString    = TCon "\ESC[33mString\ESC[0m"
typeChar      = TCon "\ESC[34mChar\ESC[0m"
typeList      = TCon "\ESC[35mList\ESC[0m"
typeEmptyList = TCon "\ESC[35mEmpty List\ESC[35m"
