{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Compiler.Failure where

import Control.Monad.Except

data Stage = Parser | TypeCheck | CodeGen

type Loc = String
type Info = String

data Failure = Failure 
          { state :: Stage
          , location :: Loc
          , summary :: Info }

tellError :: Failure -> String
tellError Failure{..} =   
    "Error: failure while " ++ stateS ++ " " ++ location ++ " " ++ summary
    where stateS = case state of
                    Parser -> "parsing" 
                    TypeCheck -> "typechecking" 
                    CodeGen -> "generating C code"

failParse :: MonadError Failure m => Loc -> Info -> m a
failParse loc info = throwError $ Failure Parser loc info

failGen :: MonadError Failure m => Loc -> Info -> m a
failGen loc info = throwError $ Failure CodeGen loc info
