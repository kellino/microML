module Language.Typing.TypeError where

import Text.PrettyPrint
import Language.Typing.Type

type Constraint = (Type, Type)

data TypeError
  = UnificationFail Type Type
  | InfiniteType TVar Type
  | UnboundVariable String
  | Ambigious [Constraint]
  | UnificationMismatch [Type] [Type]

ppError :: TypeError -> Doc
ppError (UnboundVariable s) = text s
ppError (InfiniteType t t') = text "Occurs check failed for infinite type" <+> text (show t) <+> text (show t')
