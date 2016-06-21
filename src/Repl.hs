module Repl where

import Syntax
import Parser
import Eval

import Control.Monad.State.Strict
import qualified Data.Map as Map
import System.Console.Repline
import Data.List (foldl')

data ReplState = ReplState
    { termEnv :: TermEnv }

initState :: ReplState
initState = ReplState emptyTmenv

type Repl a = HaskelineT (StateT ReplState IO) a

liftError :: Show e => Either e a -> Repl a
liftError (Right val) = return val
liftError (Left err) = do
    liftIO $ print err
    abort

evalDecl :: TermEnv -> (String, Expr) -> TermEnv
evalDecl env (var, body) = termEnv'
    where (val, termEnv') = runEval env var body
