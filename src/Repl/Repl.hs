{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Repl.Repl where

import Repl.Eval
import Repl.Pretty
import MicroML.Syntax
import MicroML.Parser
import MicroML.Lexer
import MicroML.Typing.Env as Env
import MicroML.Typing.Infer

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

hoistError :: Show e => Either e a -> Repl a
hoistError (Right val) = return val
hoistError (Left err) = do
  liftIO $ print err
  abort

evalDef :: TermEnv -> (String, Expr) -> TermEnv
evalDef env (nm, ex) = termEnv'
  where (val, termEnv') = runEval env nm ex

exec :: Bool -> L.Text -> Repl ()
exec update source = do
    st <- get

    mod <- hoistError $ parseProgram "<stdin>" source

    typeEnv' <- hoistError $ inferTop (typeEnv st) mod

    let st' = st { termEnv = foldl' evalDef (termEnv st) mod
                 , typeEnv = typeEnv' `mappend` typeEnv st 
       }

    when update (put st')

    case Prelude.lookup "it" mod of
      Nothing -> return ()
      Just ex -> do
        let (val, _) = runEval (termEnv st') "it"  ex
        --liftIO $ print val
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
browse :: [String] -> Repl ()
browse _ = do
  st <- get
  liftIO $ mapM_ putStrLn $ ppenv (typeEnv st)

-- :?
help :: [String] -> HaskelineT (Control.Monad.State.Strict.StateT IState IO) ()
help = return $ liftIO $ putStrLn "can't help you on that one"

-- :using command
using :: [String] -> Repl ()
using args = do
    --contents <- liftIO $ L.readFile $ unwords args
    contents <- liftIO $ L.readFile $ "/home/david/.microML/" ++ unwords args ++ ".ml"
    exec True contents

-- :type command
typeof :: [String] -> Repl ()
typeof args = do
  st <- get
  let arg = unwords args
  case Env.lookup arg (typeEnv st) of
    Just val -> liftIO $ putStrLn $ ppsig (arg, val)
    Nothing -> exec False $ L.pack arg

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
    let builtins = reservedNames
    return $ filter (isPrefixOf n) (cmds ++ defs ++ builtins)

options :: [(String, [String] -> Repl ())]
options = [
    ("using", using)
  , ("browse" , browse)
  , ("quit" ,  quit)
  , ("type" , typeof)
  , ("!"    , sh)
  , ("?"    , help)
  ]

-------------------------------------------------------------------------------
-- Entry Point
-------------------------------------------------------------------------------

completer :: CompleterStyle (StateT IState IO)
completer = Prefix (wordCompleter comp) defaultMatcher

prompt :: String
prompt = "\ESC[33mmicroML ⊦\ESC[0m "

-- this looks a little weird and distorted due to the necessity of escaping the \ character. But it
-- does work!
banner :: String
banner = "\ESC[1;31m" ++
        "            _               ___  ___ _       \n" ++
        "           (_)              |  \\/  || |      \n" ++
        "  _ __ ___  _  ___ _ __ ___ | .  . || |               \ESC[33;1mversion 0.01\ESC[1;31m\n" ++
        " | '_ ` _ \\| |/ __| '__/ _ \\| |\\/| || |           \ESC[33;1mfor help type :?\ESC[1;31m\n" ++
        " | | | | | | | (__| | | (_) | |  | || |____  \n" ++
        " |_| |_| |_|_|\\___|_|  \\___/\\_|  |_/\\_____/  \ESC[0m"

ini' :: Repl ()
ini' = liftIO $ putStrLn $ banner ++ "\n\n" ++ "\ESC[1mWelcome to microML\ESC[0m\n\n" 

ini :: Repl ()
ini = do
    using ["standard"]
    liftIO $ putStrLn $ banner ++ "\n\n" ++ "\ESC[1mWelcome to microML\ESC[0m\n\n" 

shell :: IO ()
shell = flip evalStateT initState $ evalRepl prompt cmd options completer ini'
