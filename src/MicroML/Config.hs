module MicroML.Config where

import Data.ConfigFile
import Data.Either.Utils
import qualified Data.Map as Map
import Control.Monad

type ConfigEnv = Map.Map OptionSpec String

configEmpty :: ConfigEnv
configEmpty = Map.empty
