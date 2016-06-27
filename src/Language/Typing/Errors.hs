module Language.Typing.Errors where

import Language.Typing.Type
import Text.PrettyPrint

data TypeError = 
        InfiniteType Type Type
      | Unsolvable Type Type
      | MiscError String
    deriving Show

output :: TypeError -> Doc
output (InfiniteType t1 t2) = text "Occurs check failed for infinite type" <+> text (show t1) 
                              <+> text "=" <+> text (show t2)
output (Unsolvable t1 t2)   = "Cannot unify types" <+> show t1 <+> text "and" <+> text (show t2)
output (MiscError s) = text s
