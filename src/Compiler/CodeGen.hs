{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Compiler.CodeGen where

import Compiler.CHelper
import Compiler.MicroBitHeader

import MicroML.Parser
import MicroML.Syntax

import qualified Text.Parsec as TP
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Trans.Either
import Control.Monad.Except
import Control.Monad.Gen 
import Language.C.DSL
import Text.Printf

import qualified Data.Text.Lazy as L
import qualified Data.Map as Map
import Data.String (fromString)

----------------
-- DATA TYPES --
----------------

data Var = SVar String | Gen Integer
    deriving (Eq, Ord, Show)

type CodeGen = WriterT [(CDecl, Maybe String, CExpr)] (StateT (Map.Map Var String) Compiler) 

type Compiler = StateT Var (EitherT Failure (Gen Integer))

data Stage = Parser | TypeCheck | CodeGen

type Loc = String
type Info = String

data Failure = Failure 
          { state :: Stage
          , location :: Loc
          , summary :: Info }

hoistError :: Show a => Either a t -> t
hoistError (Right vl) = vl
hoistError (Left err) = error $ show err

tellError :: Failure -> String
tellError Failure{..} =   
    printf "Error: failure while %s at %s. \n" stateS location summary
    where stateS = case state of
                    Parser -> "parsing" 
                    TypeCheck -> "typechecking" 
                    CodeGen -> "generating C code"

failGen :: MonadError Failure m => Loc -> Info -> m a
failGen loc info = throwError $ Failure CodeGen loc info

compileMicroML :: Either TP.ParseError [(String, Expr)] -> Compiler [CExtDecl]
compileMicroML res = 
    case res of
      Left _  -> undefined
      Right r -> codegen r

codegen :: [(String, Expr)] -> Compiler [CExtDecl]
codegen = flip evalStateT Map.empty 
        . fmap (uncurry makeMain) 
        . runWriterT 
        . mapM generate

makeMain :: [CExtDecl] -> [(CDecl, Maybe String, CExpr)] -> [CExtDecl]
makeMain decls _ = 
    export (fun [intTy] "main" [] $ hBlock [
            microBitInit
          , releaseFiber
    ]) : decls

generate :: (String, Expr) -> CodeGen CExtDecl
generate (nm, expr) = 
    let newNm = fromString nm
     in case expr of
          Lit (LInt n)     -> return $ export $ int newNm .= fromInteger n
          Lit (LDouble d)  -> return $ export $ double newNm .= realToFrac d
          Lit (LChar c)    -> return $ export $ char newNm .= chr c
          Lit (LString st) -> return $ export $ charPtr newNm .= str st
          Nil              -> return undefined
          x                -> failGen (show x) ""

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

compile :: L.Text -> L.Text -> IO ()
compile source dest = do
    let res = parseProgram "from source" source
    let compRes = runCompiler (compileMicroML res) 
    case compRes of
      Right ccode -> writeCFile dest ccode
      Left e -> putStrLn (tellError e)
