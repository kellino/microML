module Repl.HelpEnv where

import qualified Data.Map as Map
import Repl.Help

data HelpEnv = HEnv { unHelp :: Map.Map String [Markdown] }
    deriving (Eq, Show)

empty :: HelpEnv
empty = HEnv Map.empty

lookup :: String -> HelpEnv -> Maybe [Markdown]
lookup k (HEnv env) = Map.lookup k env

extend :: HelpEnv -> (String, [Markdown]) -> HelpEnv
extend env (var, ts) =  env { unHelp = Map.insert var ts (unHelp env) }

merge :: HelpEnv -> HelpEnv -> HelpEnv
merge (HEnv a) (HEnv b) = HEnv (Map.union a b)
