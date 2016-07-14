{-# LANGUAGE OverloadedStrings #-}

module Compiler.CodeGen where

import Language.C.DSL

includes :: String
includes = "#include <stdio.h>\n\n"

makeMain = 
    fun [intTy] "main" [] $ hBlock [
        "printf" # [str "hello microML"]                               
    ]

compile :: IO ()
compile = do
    let cFile = "out.c"
    writeFile cFile $ includes ++ (show . pretty $ makeMain)
