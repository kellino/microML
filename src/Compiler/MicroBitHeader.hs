{-# LANGUAGE OverloadedStrings #-}

module Compiler.MicroBitHeader where

import Language.C.DSL
import qualified Data.Map as Map

type RetTy = String

microBitIncludes :: String
microBitIncludes = "#include \"MicroBit.h\"\n\nMicroBit uBit;\n"

microBitAPI :: Map.Map String (CExpr, RetTy)
microBitAPI = Map.fromList [
    ("scroll", ("uBit.display.scroll", "void"))                       
    ]

blank :: CExpr
blank = ""

microBitInit :: CExpr
microBitInit = "uBit.init()"

releaseFiber :: CExpr
releaseFiber = "release_fiber()"
