{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Repl.Repl where

import Repl.Eval
import Repl.Pretty
import Language.Syntax
import Language.Parser
import Language.Typing.Env as Env
import Language.Typing.Infer

import qualified Data.Map as Map
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import Control.Monad.State.Strict

import Data.List (isPrefixOf, foldl')

import System.Exit
import System.Console.Repline
import qualified System.Process as S

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data IState = IState
  { typeEnv :: Env  -- Type environment
  , termEnv :: TermEnv  -- Value environment
  }

initState :: IState
initState = IState Env.empty emptyTmenv

type Repl a = HaskelineT (StateT IState IO) a

liftError :: Show e => Either e a -> Repl a
liftError (Right val) = return val
liftError (Left err) = do
  liftIO $ print err
  abort

-------------------------------------------------------------------------------
-- Execution
-------------------------------------------------------------------------------

evalDef :: TermEnv -> (String, Expr) -> TermEnv
evalDef env (nm, ex) = termEnv'
  where (val, termEnv') = runEval env nm ex

exec :: Bool -> L.Text -> Repl ()
exec update source = do
  -- Get the current interpreter state
  st <- get

  -- Parser ( returns AST )
  mod <- liftError $ parseProgram "<stdin>" source

  -- Type Inference ( returns Typing Environment )
  typeEnv' <- liftError $ inferTop (typeEnv st) mod

  -- Create the new environment
  let st' = st { termEnv = foldl' evalDef (termEnv st) mod
      , typeEnv = typeEnv' -- <> (typeEnv st)
               }

  -- Update the interpreter state
  when update (put st')

  -- If a value is entered, print it.
  case Prelude.lookup "it" mod of
    Nothing -> return ()
    Just ex -> do
      let (val, _) = runEval (termEnv st') "it"  ex
      showOutput (show val) st'

showOutput :: String -> IState -> Repl ()
showOutput arg st = 
  case Env.lookup "it" (typeEnv st)  of
    Just val -> liftIO $ putStrLn $ ppsig (arg, val)
    Nothing -> return ()

cmd :: String -> Repl ()
cmd source = exec True (L.pack source)
-------------------------------------------------------------------------------
-- Commands
-------------------------------------------------------------------------------

-- :browse command
{-browse :: [String] -> Repl ()-}
{-browse _ = do-}
  {-st <- get-}
  {-liftIO $ mapM_ putStrLn $ ppenv (typeEnv st)-}

-- :load command
using :: [String] -> Repl ()
using args = do
  contents <- liftIO $ L.readFile (unwords args)
  exec True contents

-- :type command
typeof :: [String] -> Repl ()
typeof args = do
  st <- get
  let arg = unwords args
  case Env.lookup arg (typeEnv st) of
    Just val -> liftIO $ putStrLn $ ppsig (arg, val)
    Nothing -> liftIO $ putStrLn "value not found"
    -- Nothing -> exec $ L.pack arg

-- :quit command
quit :: a -> Repl ()
quit _ = liftIO exitSuccess

-- access the shell
sh :: [String] -> Repl ()
sh arg = liftIO $ S.callCommand (unwords arg)

-------------------------------------------------------------------------------
-- Interactive Shell
-------------------------------------------------------------------------------

-- Prefix tab completer
defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher = [
    (":using"  , fileCompleter)
  ]

-- Default tab completer
comp :: (Monad m, MonadState IState m) => WordCompleter m
comp n = do
    let cmds = [":using", ":type", ":browse", ":quit", ":"]
    Env.TypeEnv ctx <- gets typeEnv
    let defs = Map.keys ctx
    return $ filter (isPrefixOf n) (cmds ++ defs)

options :: [(String, [String] -> Repl ())]
options = [
    ("using"  , using)
  --, ("browse" , browse)
  , ("quit"   , quit)
  , ("type"   , typeof)
  , ("!"  , sh)
  ]

-------------------------------------------------------------------------------
-- Entry Point
-------------------------------------------------------------------------------

completer :: CompleterStyle (StateT IState IO)
completer = Prefix (wordCompleter comp) defaultMatcher

prompt :: String
prompt = "\ESC[33mmicroML ⊦\ESC[0m "

shell :: Repl a -> IO ()
shell pre = flip evalStateT initState
     $ evalRepl prompt cmd options completer pre
