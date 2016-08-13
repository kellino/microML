{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Compiler.Failure where

import Control.Monad.Except
import Text.PrettyPrint

data Stage = Parser | TypeCheck | CodeGen 

type Loc = Doc
type Info = Doc

data Failure = Failure 
          { state    :: Stage
          , location :: Loc
          , summary  :: Info }

tellError :: Failure -> Doc
tellError Failure{..} =   
    "Error: failure while " <> stateS <> " " <> location <> " " <> summary
    where stateS = case state of
                    Parser -> "parsing" 
                    TypeCheck -> "typechecking" 
                    CodeGen -> "generating C code"

failGen :: MonadError Failure m => Loc -> Info -> m a
failGen loc info = throwError $ Failure CodeGen loc info
