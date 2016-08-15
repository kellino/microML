module Main  where

import Compiler.CodeGen 
import Compiler.CallGraph
import MicroML.Parser
import MicroML.Syntax (red, clear)
import Repl.Repl hiding (clear, hoistError)

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
    | CallGraph
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
    , Option ['g'] [] (NoArg CallGraph)
    "Produces a png image of the program's call graph (how each function is linked to the others)"
    , Option [] ["help"] (NoArg Help)
    "Prints this help message"
    ]

parseCmds :: [String] -> IO ([Flag], [String])
parseCmds argv = 
    if null argv
       then do hPutStrLn stderr $ "Please enter one of the following option:\n" ++ usageInfo header flags
               exitFailure
       else
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
      CallGraph   -> 
          if length fs /= 1 -- only accept one file at a time
             then die $ red ++ "Exit Failure: " ++ clear ++ "you must provide only one source file at a time. Sorry :("
             else do
                 contents <- LIO.readFile(head fs)
                 let res = hoistError $ parseProgram "from file" contents
                 drawGraph res
      Compiler    -> 
          if length fs /= 2  -- only accepts one file for the moment
             then die $ red ++ "Exit Failure: " ++ clear ++ "you must provide a source file and a destination file"
             else do
                 tr <- doesFileExist $ head fs
                 if tr 
                    then do
                        contents <- LIO.readFile (head fs) 
                        compile contents (L.pack $ last fs) (head fs)
                    else die $ red ++ "Exit Failure: " ++ clear ++ "the given file doesn't exist in that location, so it can't be compiled!"
      Jit         -> do -- die "The jit is not yet operable"
          exists <- findExecutable "llc"
          if null exists
             then die $ "Unable to find" ++ red  ++ " LLVM " ++ clear ++ " on your computer."
                        ++ " Are you sure it is installed?"
             else die "This jit is not yet operable"

main :: IO ()
main = do
    (args, files) <- getArgs >>= parseCmds
    microML (head args) files
