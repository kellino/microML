{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Repl.Repl where

import Repl.Eval hiding (mod')
import Repl.HelpEnv
import qualified Repl.HelpEnv as HE
import Repl.Pretty
import Repl.Help

import MicroML.Syntax
import MicroML.Parser
import MicroML.Lexer hiding (contents)
import MicroML.Typing.Env as Env
import MicroML.Typing.Infer

import qualified Data.Map as Map
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import Data.List (isPrefixOf, foldl')

import Control.Monad.State.Strict

import System.Exit
import System.Directory
import System.FilePath
import System.Console.Repline
import qualified System.Process as S

-----------
-- Types -- 
-----------

data IState = IState
      { typeEnv :: Env      -- Type environment
      , termEnv :: TermEnv  -- Value environment
      , helpEnv :: HelpEnv  -- Help environment
      }

initState :: IState
initState = IState Env.empty emptyTmenv HE.empty

type Repl a = HaskelineT (StateT IState IO) a

hoistError :: (Show a1) => Either a1 a -> Repl a
hoistError (Right val) = return val
hoistError (Left err) = do
  liftIO $ print err
  abort

ignoreError :: Either a1 a -> Repl a
ignoreError (Right val) = return val

evalDef :: TermEnv -> (String, Expr) -> TermEnv
evalDef env (nm, ex) = termEnv'
  where (_, termEnv') = runEval env nm ex

toHelpenv :: [HelpBlock] -> HelpEnv
toHelpenv ls = HEnv $ Map.fromList ls

exec :: Bool -> L.Text -> Repl ()
exec update source = do
    st       <- get

    mod'     <- hoistError $ parseProgram "<stdin>" source
    typeEnv' <- hoistError $ inferTop (typeEnv st) mod'

    let st' = st { termEnv = foldl' evalDef (termEnv st) mod'
                 , typeEnv = typeEnv' `mappend` typeEnv st 
                 }

    when update (put st')

    case Prelude.lookup "it" mod' of
      Nothing -> return ()
      Just ex -> do
        let (val, _) = runEval (termEnv st') "it" ex
        showOutput val st'

exec' :: L.Text -> Repl ()
exec' source = do
    st       <- get

    mod'     <- hoistError $ parseProgram "<stdin>" source
    typeEnv' <- hoistError $ inferTop (typeEnv st) mod'
    helpEnv' <- hoistError $ parseHelp "<from file>" source

    let st' = st { termEnv = foldl' evalDef (termEnv st) mod'
                 , typeEnv = typeEnv' `mappend` typeEnv st 
                 , helpEnv = toHelpenv helpEnv' `mappend` helpEnv st
                 }
    put st'

showOutput :: Expr -> IState -> Repl ()
showOutput arg st = 
    case Env.lookup "it" (typeEnv st) of
      Just val -> liftIO $ putStrLn $ ppsig (arg, val)
      Nothing -> return ()

cmd :: String -> Repl ()
cmd source = exec True (L.pack source) 

-------------------------------------------------------------------------------
-- Commands
-------------------------------------------------------------------------------

-- :Parse Tree
pst :: [String] -> Repl ()
pst expr = do
    tree <- hoistError $ parseProgram "<stdin>" $ L.pack (concat expr)
    tyEnv <- hoistError $ inferTop Env.empty tree
    liftIO $ putStrLn $ show tree ++ concat (ppenv tyEnv)

-- :browse command
browse :: [String] -> Repl ()
browse _ = do
  st <- get
  liftIO $ mapM_ putStrLn $ ppenv (typeEnv st)

help :: [String] -> Repl ()
help args = 
    if null args
       then liftIO $ putStrLn "you haven't entered a function"
       else do st <- get
               let arg = unwords args
               case HE.lookup arg (helpEnv st) of
                   Just val -> liftIO $ putStr $ "\n" ++ renderHelp val ++ "\n"
                   Nothing -> liftIO $ putStrLn $ "there is no help available for " ++ arg ++ " (:"

-- :using command
using :: [String] -> Repl ()
using args = 
    if null args 
       then liftIO $ putStrLn "you must enter a filename!"
       else do dir <- liftIO getHomeDirectory
               let stdlib = dir </> ".microML/"
               exists <- liftIO $ doesDirectoryExist stdlib
               if exists
                 then do 
                    contents <- liftIO $ L.readFile $ stdlib ++ unwords args ++ ".mml"
                    exec' contents 
                 else error "\ESC[31mError\ESC[0m: Unable to locate standard library in home directory"

-- :type command
typeof :: [String] -> Repl ()
typeof args = 
    if null args
       then liftIO $ putStrLn "you must enter the name of a function"
       else do
          st <- get
          let arg = unwords args
          case Env.lookup arg (typeEnv st) of
            Just val -> liftIO $ putStrLn $ ppsig' (arg, val)
            Nothing -> exec False $ L.pack arg

-- :quit command
quit :: a -> Repl ()
quit _ = liftIO exitSuccess

-- access the shell -- needs error checking
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
    let cmds = [":using", ":type", ":browse", ":quit", ":", ":help", ":?", ":pst"]
    Env.TypeEnv ctx <- gets typeEnv
    let defs = Map.keys ctx
    let builtins = reservedNames
    return $ filter (isPrefixOf n) (cmds ++ defs ++ builtins)

options :: [(String, [String] -> Repl ())]
options = [
        ("using"  , using)
      , ("browse" , browse)
      , ("quit"   , quit)
      , ("type"   , typeof)
      , ("!"      , sh)
      , ("?"      , help)
      , ("help"   , help) -- alternative
      , ("pst"    , pst) -- view parse tree of a given expression
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

ini :: Repl ()
ini = do
    using ["standard"]
    liftIO $ putStrLn $ banner ++ "\n\n" ++ "\ESC[1mWelcome to microML\ESC[0m\n\n" 

shell :: IO ()
shell = flip evalStateT initState $ evalRepl prompt cmd options completer ini
