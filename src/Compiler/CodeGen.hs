module Compiler.CodeGen where

import Compiler.MicroBitHeader
import Compiler.CallGraph
import Compiler.Failure

import MicroML.Parser
import MicroML.Syntax

import qualified Text.Parsec as TP
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Trans.Either
import Control.Monad.Gen 
import Language.C.DSL

import qualified Data.Text.Lazy as L
import qualified Data.Map as Map

----------------
-- DATA TYPES --
----------------

data Var = SVar String | Gen Integer
    deriving (Eq, Ord, Show)

type CodeGen = WriterT [(CDecl, Maybe String, CExpr)] (StateT (Map.Map Var String) Compiler) 

type Compiler = StateT Var (EitherT Failure (Gen Integer))

compileMicroML :: Either TP.ParseError [(String, Expr)] -> Compiler [CExtDecl]
compileMicroML res = 
    case res of
      Left err -> failParse (show err) ""
      Right r -> codegen r

codegen :: [(String, Expr)] -> Compiler [CExtDecl]
codegen = flip evalStateT Map.empty 
        . fmap (uncurry makeMain) 
        . runWriterT 
        . mapM generateCode
        . validDefs
        . checkForDuplicates

makeMain :: [CExtDecl] -> [(CDecl, Maybe String, CExpr)] -> [CExtDecl]
makeMain decls _ = 
    decls ++
    [export (fun [intTy] "main" [] $ hBlock [
            microBitInit
    ])] 

generateCode :: (String, Expr) -> CodeGen CExtDecl
generateCode (nm, expr) = do
    let funcName = nm
    let body = makeFuncBody expr
    return $ export $ fun [voidTy] funcName [] $ hBlock [ body ] 
    
makeFuncBody :: Expr -> CExpr
makeFuncBody (Lam x xs) = makeFuncBody xs

renderC :: [CExtDecl] -> String
renderC = unlines . map (show . pretty)

runCompiler :: Compiler a -> Either Failure a
runCompiler = runGen
             . eitherT (return . Left) (return . Right)
             . flip evalStateT (SVar "")

writeCFile :: L.Text -> [CExtDecl] -> IO ()
writeCFile dest code = do
    let cFile = L.unpack dest ++ ".cpp"
    writeFile cFile $ microBitIncludes ++ renderC code

compile :: L.Text -> L.Text -> String -> IO ()
compile source dest filename = do
    let res = parseProgram filename source
    print res -- for debugging
    let compRes = runCompiler (compileMicroML res) 
    case compRes of
      Right ccode -> writeCFile dest ccode
      Left e -> putStrLn (tellError e)
