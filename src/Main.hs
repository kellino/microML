module Main  where


import Compiler.CodeGen
import Repl.Repl

import System.IO (hPutStrLn, stderr)
import System.Exit
import System.Console.CmdArgs.GetOpt
import System.Environment (getArgs)
import System.Directory

import qualified Data.Text.Lazy.IO as LIO
import qualified Data.Text.Lazy as L
import Data.List (nub)

type File = String

data Flag = 
      Interpreter
    | Jit
    | Compiler
    | ObjectFile
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
    , Option ['o'] [] (NoArg ObjectFile)
    "The name of the new file you want to save"
    , Option [] ["help"] (NoArg Help)
    "Prints this help message"
    ]

parseCmds :: [String] -> IO ([Flag], [String])
parseCmds argv = 
    case getOpt Permute flags argv of
      (args, fs, []) -> do
          let files = if null fs then ["-"] else fs
          if Help `elem` args
             then do hPutStrLn stderr (usageInfo header flags)
                     exitSuccess
             else return (nub args, files)
      (_,_,errs) -> do
          hPutStrLn stderr (concat errs ++ usageInfo header flags)
          exitWith (ExitFailure 1)
      where header = "Usage: microML [-jcio] [file ...]"

microML :: Flag -> [FilePath] -> IO ()
microML arg fs =
    case arg of
      Interpreter -> shell
      ObjectFile  -> undefined
      Compiler    -> do
          fl <- doesFileExist $ head fs
          if fl 
             then do
               contents <- LIO.readFile $ head fs
               compile contents $ L.pack (head $ tail fs)
             else die "Exit Failure: the given file doesn't exist in that location, so it can't be compiled!"
      Jit         -> do
          putStrLn "the jit is not yet operative"
          exitWith $ ExitFailure 1

main :: IO ()
main = do
    (args, files) <- getArgs >>= parseCmds
    microML (head args) files
