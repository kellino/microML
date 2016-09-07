module MicroML.Config where

import Data.ConfigFile
import Data.Either.Utils
import qualified Data.Map as Map
import Control.Monad

type ConfigEnv = Map.Map OptionSpec String

configEmpty :: ConfigEnv
configEmpty = Map.empty

--loadConfig :: FilePath -> IO ConfigEnv
loadConfig file = do
    conf <- readfile emptyCP file
    let cp = forceEither conf
    putStrLn "Your setting is: "
    let config = forceEither $ items cp "colourscheme"
    Map.fromList config
