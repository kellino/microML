module MicroML.Config where

import Data.ConfigFile
import qualified Data.Map as Map
import Control.Monad.Except
import System.FilePath
import System.Directory


dark :: Map.Map String String
dark = Map.fromList 
    [ ("bold", "\ESC[1m")
    , ("clear", "\ESC[0m")
    ]

data ConfigInfo = ConfigInfo { colourScheme :: String }
                deriving Show

readConfig :: String -> IO ConfigInfo
readConfig f = do
    rv <- runExceptT $ do
        cp <- join $ liftIO $ readfile emptyCP f
        let x = cp

        cs <- get x "ColourScheme" "terminal"

        return ConfigInfo { colourScheme = cs }

    either (error . snd) return rv

{-main :: IO ()-}
{-main = do-}
    {-home <- getHomeDirectory-}
    {-conf <- readConfig $ home </> ".microMLrc"-}
    {-print conf-}
