{-# LANGUAGE OverloadedStrings #-}

module Compiler.MicroBitHeader where

import Text.PrettyPrint
import qualified Data.Map as Map

-- | this needs to be vastly extended to cope with the entire API. TODO
-- this covers the basic display module only at the moment
microBitAPI :: Map.Map String Doc
microBitAPI = Map.fromList [
    ("scroll", "uBit.display.scroll")
  , ("print", "uBit.display.print" )
  , ("animate", "uBit.display.animate")
  , ("setBrightness", "uBit.display.setBrightness")
  , ("rotateTo", "uBit.display.rotateTo")
    ]

microBitIncludes :: Doc
microBitIncludes = "#include \"MicroBit.h\"\n\nMicroBit uBit;\n"

bitInit :: Doc
bitInit = "ubit.init();\n"

fiber :: Doc
fiber = "fiber_release();\n"
