{-# LANGUAGE OverloadedStrings #-}

module CTest  where

import Language.C.DSL

standardHeader :: String
standardHeader = "#include \"MicroBit.h\"\nMicroBit uBit;\n\n"

makeMain = fun [intTy] "main"[] $ hBlock [ ]
