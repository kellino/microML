module Main  where

--import MicroML.Parser
import Repl.Repl
--import Jit.Codegen
--import Jit.Emit

--import Control.Monad (void)
import System.Environment (getArgs)
--import qualified Data.Text.Lazy as L

--import qualified LLVM.General.AST as AST

{-initModule :: AST.Module-}
{-initModule = emptyModule "microMLJit"-}

{-process :: AST.Module -> String -> IO (Maybe AST.Module)-}
{-process modo source = do-}
    {-let res = parseProgram source-}
    {-case res of-}
      {-Left err -> void $ print err-}
      {-Right ex -> do-}
          {-ast <- codegen-}
          {-return $ Just ast-}

--processFile fn = L.readFile fn >>= process initModule

main :: IO ()
main = do
    args <- getArgs
    case args of
      []    -> shell
      --[fname] -> void $ processFile fname 
