{-# LANGUAGE FlexibleContexts #-}

module Repl where

import Eval

import Control.Monad.Trans
import System.Console.Repline
import Data.List (isPrefixOf)
import System.IO


type Repl a = HaskelineT IO a

cmd :: String -> Repl ()
cmd input = liftIO $ process input

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
--prompt = "\ESC[33mmicroML ⊦\ESC[0m "
prompt = "\ESC[33mmicroML :→\ESC[0m "

repl :: IO ()
repl = evalRepl prompt cmd options (Word completer) initialize
