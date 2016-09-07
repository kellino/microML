module MicroML.Config (loadConfig) where

import Data.ConfigFile
import Data.Either.Utils

loadConfig :: FilePath -> IO ()
loadConfig file = do
    conf <- readfile emptyCP file
    let cp = forceEither conf
    putStrLn "Your setting is: "
    let config = forceEither $ items cp "colourscheme"
    print config

{-main :: IO ()-}
{-main = config-}
