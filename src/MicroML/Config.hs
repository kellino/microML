module MicroML.Config where

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
