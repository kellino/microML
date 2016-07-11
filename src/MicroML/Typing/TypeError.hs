module MicroML.Typing.TypeError where

import MicroML.Typing.Type

type Constraint = (Type, Type)

data TypeError
  = UnificationFail Type Type
  | InfiniteType TVar Type
  | UnboundVariable String
  | Ambigious [Constraint]
  | UnificationMismatch [Type] [Type]
