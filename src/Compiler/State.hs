{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Compiler.State where

import MicroML.Syntax

import Control.Monad.RWS
import qualified Data.Map as Map

newtype Serial = Serial Integer deriving Show

newtype Compiler a = Compiler { unCompiler :: RWS Scope [Decl] Serial a}
    deriving (Functor, Applicative, Monad)

data Scope = Scope { liftedMap :: Map.Map Name Expr }

initScope :: Scope
initScope = Scope { liftedMap = Map.empty }

-- generate a fresh name for a variable
fresh :: Compiler Integer
fresh = Compiler $ do
    Serial i <- get
    put (Serial (succ i))
    return i

newVar :: Compiler Name
newVar = do
    i <- fresh
    return $ "_v" ++ show i

runCompiler :: r -> RWS r w Scope a -> (a, w)
runCompiler env m = evalRWS m env initScope
