module Language.Typing.TypeError where

import Text.PrettyPrint
import Language.Typing.Type

data TypeError 
    = InfiniteType Type Type
  | UnboundVariable String
  | Unsolvable Type Type
    deriving (Show)

ppError :: TypeError -> Doc
ppError (UnboundVariable s) = text s
ppError (InfiniteType t t') = text "Occurs check failed for infinite type" <+> text (show t) <+> text (show t')
ppError (Unsolvable t t')   = text "Cannot unify types" <+> text (show t) <+> text "and" <+> text (show t')
