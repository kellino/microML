module MicroML.Config where

import MicroML.Syntax (clear)

import Data.ConfigFile
import System.Terminfo
import qualified System.Terminfo.Caps as C
import System.Environment (lookupEnv)
import Data.Maybe (fromJust)
import qualified Data.Map as Map

type ConfigEnv = Map.Map OptionSpec String

configEmpty :: ConfigEnv
configEmpty = Map.empty

maxColours :: IO (Maybe Int)
maxColours = do
    term <- fromJust <$> lookupEnv "TERM"
    db <- acquireDatabase term
    let colours (Right d) = queryNumTermCap d C.MaxColors
    return $ colours db

escapeCode :: String -> String
escapeCode "16" = "\ESC["
escapeCode "255" = "\ESC[38;5;"
escapeCode _ = error "unsupported terminal type"

getColour :: ConfigEnv -> String -> String 
getColour env colour = fromJust (Map.lookup colour env)

escape :: [(String, String)] -> String -> [(String, String)]
escape xs term = map (\(x,y) -> if x == "bold" then new x y esc' else new x y esc) xs 
    where esc = escapeCode term
          esc' = "\ESC["
          new x y code = (x, code ++ y ++ "m") 

-- | an unfortunate function brought about by the fact that I only thought about adding a
-- configuration file near to the project deadline. It would be much better to wrap this all in some
-- prettyprint monad of some sort. That's another element in the TODO list...
putColour :: ConfigEnv -> String -> String
putColour conf = unwords . pc . words
    where pc [] = []
          pc (x:xs) 
              | x == "Number" = (getColour conf "number" ++ x ++ clear) : pc xs
              | x == "(Number" = ('(' : (getColour conf "number" ++ "Number" ++ clear)) : pc xs
              | x == "Number)" = ((getColour conf "number" ++ "Number" ++ clear) ++ ")") : pc xs

              | x == "String" = (getColour conf "string" ++ x ++ clear) : pc xs
              | x == "(String" = ('(' : (getColour conf "string" ++ x ++ clear)) : pc xs
              | x == "String)" = ((getColour conf "string" ++ x ++ clear) ++ ")") : pc xs

              | x == "Char" = (getColour conf "char" ++ x ++ clear) : pc xs
              | x == "(Char" = ('(' : (getColour conf "char" ++ x ++ clear)) : pc xs
              | x == "Char)" = ((getColour conf "char" ++ x ++ clear) ++ ")") : pc xs

              | x == "Boolean" = (getColour conf "boolean" ++ x ++ clear) : pc xs
              | x == "(Boolean" = ('(' : (getColour conf "boolean" ++ "Boolean" ++ clear)) : pc xs
              | x == "Boolean)" = (getColour conf "boolean" ++ "Boolean" ++ clear ++ ")") : pc xs

              | x == "Error" = (getColour conf "error" ++ x ++ clear) : pc xs
              | x == "→" = (getColour conf "arrow" ++ x ++ clear) : pc xs
              | otherwise    = x : pc xs
