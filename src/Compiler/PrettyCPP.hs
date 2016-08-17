{-# LANGUAGE OverloadedStrings #-}

module Compiler.PrettyCPP where

import MicroML.Syntax
import Text.PrettyPrint

import System.Directory
import System.Process
import System.IO
import Control.Exception (catch, IOException)


-- use clang-format, if installed, to render nice cpp, otherwise leave it ugly
formatPrintedFile fl = do
    clang <- findExecutable "clang-format" 
    case clang of
         Nothing -> putStr ""
         Just _ -> 
            catch (callCommand $ "clang-format " ++ fl)
                  (\e -> do let err = show (e :: IOException)
                            hPutStr stderr ("Clang-format was unable to reformat" ++ fl ++ "\n" ++ err ++ "\n")
                            return ())

showText :: Show a => a -> Doc 
showText = text . show

semiWithNewLine :: Doc
semiWithNewLine = semi <> "\n"

parensWithSemi :: Doc -> Doc
parensWithSemi d = parens d <> semi <> "\n"

bracesNewLine :: Doc -> Doc
bracesNewLine d = braces ("\n\t" <> d) <> "\n"

class Pretty p where
    ppr :: p -> Doc

instance Pretty Binop where 
    ppr OpAdd    = " + "
    ppr OpSub    = " - "
    ppr OpMul    = " * "
    ppr OpDiv    = " / "
    ppr OpIntDiv = " / "
    ppr OpMod    = " % "
    ppr OpOr     = " || "
    ppr OpAnd    = " && "
    ppr OpNotEq  = " != "
    ppr OpEq     = " == "
    ppr OpExp    = "^"
    ppr OpLe     = " <= "
    ppr OpLt     = " < "
    ppr OpGe     = " >= "
    ppr OpGt     = " > "
    ppr OpXor    = undefined
    ppr OpAppend = " + "
