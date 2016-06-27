module Language.Typing.Errors where

import Language.Typing.Type
--import Text.PrettyPrint

data TypeError = 
        InfiniteType Type Type
      | Unsolvable Type Type
      | MiscError String
    deriving Show

output :: TypeError -> String
output (InfiniteType t1 t2) = "Occurs check failed for infinite type " ++ show t1 ++ " = "  ++ show t2
output (Unsolvable t1 t2)   = "Cannot unify types " ++ show t1 ++ " and " ++ show t2
output (MiscError s)        = s
