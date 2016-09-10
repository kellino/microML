{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Repl.Repl where

import MicroML.Config
import Repl.Eval 
import Repl.HelpEnv
import qualified Repl.HelpEnv as HE
import Repl.Pretty
import Repl.ParseTree
import Repl.Help

import MicroML.Syntax as S
import MicroML.Parser
import MicroML.Lexer hiding (contents)
import MicroML.Typing.Env as Env
import MicroML.Typing.Infer

import qualified Data.Map as Map
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import Data.List (isPrefixOf, foldl', isInfixOf)
import qualified Data.ConfigFile as DC
import Data.Either.Utils
import Data.Maybe (fromJust)

import Control.Monad.State.Strict
import Control.Exception (catch, IOException)

import System.IO
import System.Exit
import System.Directory
import System.FilePath
import System.Console.Repline
import qualified System.Process as S

-----------
-- Types -- 
-----------

data IState = IState
      { typeEnv :: Env         -- Type environment
      , termEnv :: TermEnv     -- Value environment
      , helpEnv :: HelpEnv     -- Help environment
      , configEnv :: ConfigEnv -- Config environment
      }

initState :: IState
initState = IState Env.microbit emptyTmenv HE.empty configEmpty

type Repl a = HaskelineT (StateT IState IO) a

hoistError :: (Show a1) => Either a1 a -> Repl a
hoistError (Right val) = return val
hoistError (Left err) = do
    liftIO $ print err
    abort

evalDef :: TermEnv -> (String, Expr) -> TermEnv
evalDef env (nm, ex) = termEnv'
  where (_, termEnv') = runEval env nm ex

-- read the help info into a dictionary
toHelpenv :: [HelpBlock] -> HelpEnv
toHelpenv ls = HEnv $ Map.fromList ls

-- | execution function while repl is running
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

-- | execution function for initial loading
-- TODO: a lot of code repetition here, should be merged with exec
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
      Just val -> liftIO $ putStrLn $ putColour (configEnv st) $ ppsig (arg, val) (configEnv st)
      Nothing -> return ()

cmd :: String -> Repl ()
cmd source = exec True (L.pack source) 

--------------
-- Commands --
--------------

-- | view the parse tree of an expression in the repl
-- :pst command
pst :: [String] -> Repl ()
pst expr = do
    st <- get
    tree <- hoistError $ parseProgram "<stdin>" $ L.pack $ concatMap (++ " ") expr
    let tyEnv = inferTop (typeEnv st) tree
    case tyEnv of
         Left err -> liftIO . print $ err
         Right _ -> liftIO . showTree . head $ tree

pstText :: [String] -> Repl ()
pstText expr = do
    st <- get
    tree <- hoistError $ parseProgram "<stdin>" $ L.pack $ concatMap (++ " ") expr
    let tyEnv = inferTop (typeEnv st) tree
    case tyEnv of
         Left err -> liftIO . print $ err 
         Right _ -> do  --  liftIO . putStrLn . head $ tree
             liftIO . putStrLn $ "The parsetree of " ++ bold ++ (fst . head) tree ++ S.clear ++ " is: "
             liftIO . putStrLn $ show . snd . head $ tree

-- :browse command
browse :: [String] -> Repl ()
browse _ = do
  st <- get
  liftIO $ mapM_ (putStrLn . putColour (configEnv st)) $ ppenv (typeEnv st) (configEnv st)

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
-- this is only for files kept in the standard library
using :: [String] -> Repl ()
using args = 
    if null args 
       then liftIO $ putStrLn "you must enter a library name!"
       else do dir <- liftIO getHomeDirectory
               let stdlib = dir </> ".microML/"
               exists <- liftIO $ doesDirectoryExist stdlib
               if exists
                  then do
                      let safe = fst (splitExtension $ unwords args) ++ ".mml"
                      tr <- liftIO $ doesFileExist $ stdlib ++ safe
                      if tr 
                         then do 
                            contents <- liftIO $ L.readFile $ stdlib ++ safe
                            exec' contents
                         else liftIO . putStrLn $ "the file " ++ unwords args ++ " does not exist"
                  else error "Error: Unable to locate standard library in the home directory"

-- :load command
load :: [String] -> Repl ()
load args =
    if null args
       then liftIO $ putStrLn "you must enter a filename"
       else do 
            tr <- liftIO $ doesFileExist (unwords args)
            if tr then do
                    contents <- liftIO $ L.readFile (unwords args)
                    exec True contents
                  else liftIO $ putStrLn "the file does not exist"

-- :type command
typeof :: [String] -> Repl ()
typeof args = 
    if null args
       then liftIO $ putStrLn "you must enter the name of a function"
       else do
          st <- get
          let arg = unwords args
          case Env.lookup arg (typeEnv st) of
            Just val -> liftIO $ putStrLn $ putColour (configEnv st) $ ppsig' (configEnv st) (arg, val)
            Nothing  -> liftIO $ putStrLn $ "microML: " ++ show arg ++ " is not in scope"

-- :quit command
quit :: a -> Repl ()
quit _ = liftIO exitSuccess

-- :clear
clear :: a -> Repl ()
clear _ = liftIO $ S.callCommand "clear"

-- :! access the shell, errors are wrapped in an IOException
sh :: [String] -> Repl ()
sh arg = liftIO $ 
    catch (S.callCommand (unwords arg))
          (\e -> do let err = show (e :: IOException)
                    hPutStr stderr ("Warning: Couldn't run " ++ unwords arg ++ " " ++ err ++ "\n")
                    return ()) 

-----------------------
-- Interactive Shell --
-----------------------

-- Prefix tab completer
defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher = [
    (":load"  , fileCompleter)
  ]

-- Default tab completer
comp :: (Monad m, MonadState IState m) => WordCompleter m
comp n = do
    let cmds = [":using", ":type", ":browse", ":quit", ":!", ":help", ":?", ":pst", ":clear", ":load", ":pstText"]
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
      , ("clear"  , Repl.Repl.clear)
      , ("?"      , help)
      , ("help"   , help) -- alternative
      , ("pst"    , pst) -- view parse tree of a given expression
      , ("pstText", pstText)
      , ("load"   , load)
      ]

-----------------
-- Entry Point --
-----------------

completer :: CompleterStyle (StateT IState IO)
completer = Prefix (wordCompleter comp) defaultMatcher

prompt :: String
prompt = "\ESC[33mmicroML ⊦ " ++ S.clear

getBanner :: Repl ()
getBanner = do
    _ <- liftIO $ S.system "figlet -f $(ls /usr/share/figlet/fonts/*.flf |shuf -n1) \"microML\"\
                         \| cowsay -n -f $(ls /usr/share/cows | shuf -n1) | lolcat"
    return ()

standardBanner :: String
standardBanner = "\ESC[1;31m" ++
        "            _               ___  ___ _       \n" ++
        "           (_)              |  \\/  || |      \n" ++
        "  _ __ ___  _  ___ _ __ ___ | .  . || |           \ESC[33;1mversion 0.05\ESC[1;31m\n" ++
        " | '_ ` _ \\| |/ __| '__/ _ \\| |\\/| || |           \ESC[33;1mfor help type :? or :help\ESC[1;31m\n" ++
        " | | | | | | | (__| | | (_) | |  | || |____  \n" ++
        " |_| |_| |_|_|\\___|_|  \\___/\\_|  |_/\\_____/  \ESC[0m"

-- | initialize the repl environment. Look for the dependencies for the fancy banner, and if not,
-- use the boring standard one.
ini :: Repl ()
ini = do
    fig <- liftIO $ findExecutable "figlet"
    cow <- liftIO $ findExecutable "cowsay"
    lol <- liftIO $ findExecutable "lolcat"
    if not (null fig) && not (null cow) && not (null lol)
       then do
           using ["standard"]
           liftIO $ putStrLn "\n\ESC[1mWelcome to microML\ESC[0m\t\t\t\ESC[33;1mversion 0.05\ESC[1;31m\n"
           getBanner 
           liftIO $ putStrLn "\n\n"
           getConfig
       else do
          using ["standard"]
          liftIO $ putStrLn $ standardBanner ++ "\n\n" ++ bold ++ "Welcome to microML" ++ S.clear ++ "\n\n"
          getConfig

-- | reads the config file (if it exists) and stores it in the global state
-- also queries terminfo for max colours supported
getConfig :: Repl ()
getConfig = do
    home <- liftIO getHomeDirectory 
    let file = home </> ".microMLrc"
    exists <- liftIO $ doesFileExist file
    if exists
       then do
           st <- get
           conf <- liftIO $ DC.readfile DC.emptyCP file
           let cp = forceEither conf
           let config = forceEither $ DC.items cp "colourscheme"
           c <- liftIO maxColours
           let term = show $ fromJust c
           let config' = config ++ [("term", term)]
           let escaped = escape config' term
           let st' = st { configEnv = Map.fromList escaped `mappend` configEnv st }
           put st'
       else error "Error: no configuration file found"

-- | main function for the repl
shell :: IO ()
shell = flip evalStateT initState $ evalRepl prompt cmd options completer ini
