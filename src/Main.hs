module Main  where


import Compiler.CodeGen
import Repl.Repl

import System.IO (hPutStrLn, stderr)
import System.Exit
import System.Console.CmdArgs.GetOpt
import System.Environment (getArgs)

import qualified Data.Text.Lazy.IO as LIO
import Data.List (nub)

type File = String

data Flag = 
      Interpreter
    | Jit
    | Compiler
    | Help
    deriving (Eq, Ord, Enum, Show, Bounded)

flags :: [OptDescr Flag]
flags = 
    [ Option ['j'] [] (NoArg Jit)
    "Runs the specified file(s) in the JIT compiler"
    , Option ['c'] [] (NoArg Compiler)
    "Compiles the specified file(s) to C++ for the bbc:microbit"
    , Option ['i'] [] (NoArg Interpreter)
    "Starts the microML interactive environment" 
    , Option [] ["help"] (NoArg Help)
    "Prints this help message"
    ]

parseCmds argv = 
    case getOpt Permute flags argv of
      (args, fs, []) -> do
          let files = if null fs then ["-"] else fs
          if Help `elem` args
             then do hPutStrLn stderr (usageInfo header flags)
                     exitWith ExitSuccess
             else return (nub args, files)
      (_,_,errs) -> do
          hPutStrLn stderr (concat errs ++ usageInfo header flags)
          exitWith (ExitFailure 1)
      where header = "Usage: microML [-jci] [file ...]"

microML :: Flag -> FilePath -> IO ()
microML arg fs =
    case arg of
      Interpreter -> shell
      Compiler    -> do
          contents <- LIO.readFile fs
          compile contents
      Jit         -> do
          putStrLn "the jit is not yet operative"
          exitWith $ ExitFailure 1

main :: IO ()
main = do
    (args, files) <- getArgs >>= parseCmds
    microML (head args) (head files)
