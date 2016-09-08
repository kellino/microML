module MicroML.Config where

import Data.ConfigFile
import Data.Either.Utils
import qualified Data.Map as Map

type ConfigEnv = Map.Map OptionSpec String

configEmpty :: ConfigEnv
configEmpty = Map.empty
