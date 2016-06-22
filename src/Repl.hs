module Repl where

import Syntax
import Parser
import Eval

import Control.Monad.State.Strict
import System.Console.Repline
import Data.List (foldl', isPrefixOf)
import System.IO

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

exec :: String -> Repl ()
exec code = do
    st <- get
    new <- liftError $ parseProgram "<stdin>" code
    let st' = st { termEnv = foldl' evalDecl (termEnv st) new }
    put st'
    case lookup "it" new of
      Nothing -> return ()
      Just x -> do
          let (val, _) = runEval (termEnv st') "it" x
          liftIO $ print val


cmd = exec

completer :: Monad m => String -> m [String]
completer n = do
    let comps = ["true", "false", "and", "or", "not", "otherwise"]
    return $ filter (isPrefixOf n) comps

options :: [(String, [String] -> Repl ())]
options = []

initialize :: Repl ()
initialize = liftIO getArt

getArt :: IO ()
getArt = do
    file <- openFile "/home/david/Programming/Haskell/microML/utils/microArt" ReadMode
    contents <- hGetContents file
    putStr $ "\ESC[1mWelcome to..." ++ "\n\n" ++ contents ++ "\ESC[0m"
    hClose file

prompt :: String
prompt = "\ESC[33mmicroML ⊦\ESC[0m "

repl :: HaskelineT (StateT ReplState IO) a -> IO ()
repl pre = flip evalStateT initState $ evalRepl prompt cmd options (Word completer) pre
