module Language.Typing.Infer where

import Language.Typing.Env
import Language.Typing.Type
import Language.Typing.Errors
import Language.Syntax

import Control.Monad.Except

type Infer a = (RWST
                Env
                [Constraint]
                InferState
                (Except TypeError) 
                a)

data InferState = InferState { count :: Int }

type Constraint = (Type, Type)

