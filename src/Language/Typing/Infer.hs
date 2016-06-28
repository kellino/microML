{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Typing.Infer  where

import Language.Typing.Type
import Language.Syntax

import Data.String
import Control.Monad.Except
import qualified Unbound.LocallyNameless as NL
import Unbound.LocallyNameless hiding (Subst, compose)

$(derive [''Type, ''Expr])

instance IsString Expr where
    fromString = Var . fromString
instance IsString Type where
    fromString = TVar . fromString
