{-# LANGUAGE OverloadedStrings #-}

module Compiler.MicroBitHeader where

import Language.C.DSL
import qualified Data.Map as Map


microBitIncludes :: String
microBitIncludes = "#include \"MicroBit.h\"\n\nMicroBit uBit;\n"

microBitAPI :: Map.Map String CExpr
microBitAPI = Map.fromList [
    ("scroll", scroll)                       
    ]

scroll :: CExpr
scroll = "uBit.display.scroll" 

releaseFiber :: CExpr
releaseFiber = "release_fiber()"
