{-# Language FlexibleInstances #-}
{-# Language TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Repl.Pretty  where

import MicroML.Typing.Type
import MicroML.Syntax
import MicroML.Typing.Env
import MicroML.Typing.TypeError

import qualified Data.Map as Map
import Text.PrettyPrint

parensIf ::  Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

class Pretty p where
    ppr :: Int -> p -> Doc

instance Pretty Name where
    ppr _  = text 

instance Pretty TVar where
    ppr _ (TV x) = text x

instance Pretty Type where
    ppr p (TArrow a b) = parensIf (isArrow a) (ppr p a) <+> text "\ESC[37m→\ESC[0m" <+> ppr p b  
        where
          isArrow TArrow{} = True
          isArrow _ = False
    ppr p (TVar a) = ppr p a
    ppr _ (TCon a) = text a

instance Pretty TypeScheme where
    ppr p (Forall [] t) = ppr p t
    ppr p (Forall ts t) = text "for all" <+> hcat (punctuate space (map (ppr p) ts)) <> text "." <+> ppr p t

instance Pretty Expr where
    ppr _ Closure{}              = text "<<closure>>"
    ppr _ (Lit (LInt i))         = integer i
    ppr _ (Lit (LDouble d))      = double d
    ppr _ (Lit (LString str))    = text str
    ppr _ (Lit (LChar c))        = text [c]        -- convert char to a string
    ppr _ (Lit (LBoolean True))  = text "true"
    ppr _ (Lit (LBoolean False)) = text "false"
    ppr _ Nil                    = text "empty list"
    ppr _ ls@(BinOp OpCons _ _)  = text $ matchBrackets (ppList ls)
    ppr _ xs                     = text $ show xs

instance Show TypeError where
      show (UnificationFail a b) =
        concat ["Cannot \ESC[1munify\ESC[0m types: ", pptype a, " with ", pptype b]
      show (InfiniteType (TV a) b) =
        concat ["Cannot construct the \ESC[0minfinite\ESC[0m type: ", a, " = ", pptype b]
      show (Ambigious cs) =
        concat ["Cannot not match expected type: '" ++ pptype a ++ "' with actual type: '" ++ pptype b ++ "'\n" | (a,b) <- cs]
      show (UnboundVariable a) = "Not in scope: " ++ a
      show (UnsupportedOperation a) = "\ESC[1m" ++ a ++ "\ESC[0m"
      show (UnificationMismatch a b) = show a ++ show b

ppscheme :: TypeScheme -> String
ppscheme = render . ppr 0

pptype :: Type -> String
pptype = render . ppr 0

-- WIP: doesn't quite work as expected on nested lists --
ppList :: Expr -> String
ppList (BinOp OpCons x Nil) = ppList x ++ "]"
ppList (BinOp OpCons Nil (BinOp OpCons x xs)) = "[" ++ ppList x ++ ppList xs
ppList (BinOp OpCons x xs)  = ppList x ++ ", " ++ ppList xs
ppList l@Lit{}           = 
    case l of
      Lit (LInt n)         -> show n
      Lit (LDouble n)      -> show n
      Lit (LString n)      -> show n
      Lit (LChar n)        -> show n
      Lit (LBoolean True)  -> "true"
      Lit (LBoolean False) -> "false"
ppList x                 = show x

matchBrackets :: String -> String
matchBrackets st = let len = length . takeWhile (== ']') . reverse $ st
                  in replicate len '[' ++ st

ppexpr :: Expr -> String
ppexpr = render . ppr 0

ppsig :: (Expr, TypeScheme) -> String
ppsig (a, b) = bold ++ ppexpr a ++ unbold ++ " : " ++ ppscheme b

ppsig' :: (String, TypeScheme) -> String
ppsig' (a, b) = bold ++ a ++ unbold ++ " : " ++ ppscheme b

ppenv :: Env -> [String]
ppenv (TypeEnv env) = map ppsig' $ Map.toList env

bold, unbold :: String
bold = "\ESC[37m"
unbold = "\ESC[0m"
