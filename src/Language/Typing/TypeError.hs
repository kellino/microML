module Language.Typing.TypeError where

data TypeError 
    = TError String
  | UnboundVariable String
    deriving (Show)


