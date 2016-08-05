{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Repl.Repl where

import Repl.Eval hiding (mod')
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

data HelpEnv = HEnv { helps :: Map.Map String [Markdown]}
    deriving (Eq, Show)

hEmpty :: HelpEnv
hEmpty = HEnv Map.empty

hLookup :: Name -> HelpEnv -> Maybe [Markdown]
hLookup k (HEnv env) = Map.lookup k env

instance Monoid HelpEnv where
    mempty = hEmpty
    mappend (HEnv a) (HEnv b) = HEnv (Map.union a b)

data IState = IState
  { typeEnv :: Env  -- Type environment
  , termEnv :: TermEnv  -- Value environment
    , helpEnv :: HelpEnv -- Help environment
  }

initState :: IState
initState = IState Env.empty emptyTmenv hEmpty

type Repl a = HaskelineT (StateT IState IO) a

hoistError :: (Show a1, MonadIO m) => Either a1 a -> HaskelineT m a
hoistError (Right val) = return val
hoistError (Left err) = do
  liftIO $ print err
  abort

evalDef :: TermEnv -> (String, Expr) -> TermEnv
evalDef env (nm, ex) = termEnv'
  where (_, termEnv') = runEval env nm ex

readHelp :: HelpEnv -> [HelpBlock] -> Either String HelpEnv
readHelp env [] = Right env
readHelp env ((nm, bdy):xs) = readHelp (hExtend env (nm, bdy)) xs

hExtend :: HelpEnv -> (String, [Markdown]) -> HelpEnv
hExtend env (func, bdy) = env { helps = Map.insert func bdy (helps env)}

exec :: Bool -> L.Text -> Repl ()
exec update source = do
    st       <- get

    mod'     <- hoistError $ parseProgram "<stdin>" source
    typeEnv' <- hoistError $ inferTop (typeEnv st) mod'
    helpEnv' <- 
        case parseHelp "<from file>" source of
          Left _ -> error "unable to open file"
          Right r -> hoistError $ readHelp (helpEnv st) r

    let st' = st { termEnv = foldl' evalDef (termEnv st) mod'
                 , typeEnv = typeEnv' `mappend` typeEnv st 
                 , helpEnv = helpEnv' `mappend` helpEnv st 
                 }

    when update (put st')

    case Prelude.lookup "it" mod' of
      Nothing -> return ()
      Just ex -> do
        let (val, _) = runEval (termEnv st') "it" ex
        showOutput val st'

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

-- :browse command
browse :: [String] -> Repl ()
browse _ = do
  st <- get
  liftIO $ mapM_ putStrLn $ ppenv (typeEnv st)

help :: [String] -> Repl ()
help _ = do
    st <- get
    liftIO $ print $ helpSize (helpEnv st)
        where helpSize (HEnv env) = Map.size env


{-help :: [String] -> Repl ()-}
{-help _ = do-}
    {-st <- get-}
    {-liftIO $ mapM_ putStr $ ppHelp (helpEnv st)-}
        {-where-}
            {-ppHelp :: HelpEnv -> [String]-}
            {-ppHelp (HEnv env) = map renderHelp $ Map.toList env-}

-- :using command
using :: [String] -> Repl ()
using args = do
    dir <- liftIO getHomeDirectory
    let stdlib = dir </> ".microML/"
    exists <- liftIO $ doesDirectoryExist stdlib
    if exists
       then do 
           contents <- liftIO $ L.readFile $ stdlib ++ unwords args ++ ".ml"
           exec True contents 
       else error "\ESC[31mError\ESC[0m: Unable to locate standard library in home directory"

-- :type command
typeof :: [String] -> Repl ()
typeof args = do
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
    let cmds = [":using", ":type", ":browse", ":quit", ":", ":help", ":?"]
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
    using ["test"]
    liftIO $ putStrLn $ banner ++ "\n\n" ++ "\ESC[1mWelcome to microML\ESC[0m\n\n" 

shell :: IO ()
shell = flip evalStateT initState $ evalRepl prompt cmd options completer ini
