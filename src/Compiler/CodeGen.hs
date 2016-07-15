{-# LANGUAGE OverloadedStrings #-}

module Compiler.CodeGen where

import Language.C.DSL
import qualified Data.Text.Lazy as L
import MicroML.Parser

hoistError :: (Monad m, Show a1) => Either a1 a -> m a
hoistError (Right val) = return val
hoistError (Left err) = error $ show err

exec :: L.Text -> String
exec source = do
    res <- hoistError $ parseProgram "<from_file>" source
    show res

includes :: String
includes = "#include <stdio.h>\n\n"

makeMain :: CFunDef
makeMain = 
    fun [intTy] "main" [] $ hBlock [
        "printf" # [str "hello microML"]                               
    ]

compile :: L.Text -> IO ()
compile file = do
    let cFile = "out.c"
    writeFile cFile $ includes ++ (show . pretty $ makeMain) ++ exec file
