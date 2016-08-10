{-# LANGUAGE OverloadedStrings #-}

module Compiler.MicroBitHeader where

import Text.PrettyPrint
import qualified Data.Map as Map


microBitAPI :: Map.Map String Doc
microBitAPI = Map.fromList [
    ("scroll", "uBit.display.scroll")
  , ("print", "uBit.display.print" )
    ]

microBitIncludes :: Doc
microBitIncludes = "#include \"MicroBit.h\"\n\nMicroBit uBit;\n"

bitInit :: Doc
bitInit = "ubit.init();\n"

fiber :: Doc
fiber = "fiber_release();\n"
