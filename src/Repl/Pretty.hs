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
import Data.List (isPrefixOf, isInfixOf) -- , intercalate)
import qualified Data.Text.Lazy as L

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
    ppr p (TArr a b) = parensIf (isArrow a) (ppr p a) <+> text "\ESC[37m→\ESC[0m" <+> ppr p b  
        where
          isArrow TArr{} = True
          isArrow _ = False
    ppr p (TVar a) = ppr p a
    ppr _ (TCon a) = text a

instance Pretty TypeScheme where
  ppr p (Forall [] t) = ppr p t
  ppr p (Forall ts t) = text "for all" <+> hcat (punctuate space (map (ppr p) ts)) <> text "." <+> ppr p t

instance Pretty Lit where
  ppr _ (LInt i) = integer i
  ppr _ (LDouble d) = double d
  ppr _ (LString str) = text str
  ppr _ (LChar c) = text [c]        -- convert char to a string
  ppr _ (LBoolean True) = text "true"
  ppr _ (LBoolean False) = text "false"

instance Show TypeError where
  show (UnificationFail a b) =
    concat ["Cannot \ESC[1munify\ESC[0m types: ", pptype a, " with ", pptype b]
  show (InfiniteType (TV a) b) =
    concat ["Cannot construct the \ESC[0minfinite\ESC[0m type: ", a, " = ", pptype b]
  show (Ambigious cs) =
    concat ["Cannot not match expected type: '" ++ pptype a ++ "' with actual type: '" ++ pptype b ++ "'\n" | (a,b) <- cs]
  show (UnboundVariable a) = "Not in scope: " ++ a
  show (UnsupportedOperatation a) = "\ESC[1m" ++ a ++ "\ESC[0m"

ppscheme :: TypeScheme -> String
ppscheme = render . ppr 0

pptype :: Type -> String
pptype = render . ppr 0

ppsig :: (String, TypeScheme) -> String
ppsig (a, b) = ppLit a ++ "\ESC[35m:\ESC[0m " ++ ppscheme b

ppenv :: Env -> [String]
ppenv (TypeEnv env) = map ppsig $ Map.toList env

-- fairly nasty and hackish this function, but it works
ppLit :: String -> String
ppLit a 
  | "Var" `isPrefixOf` a     = bold ++ ((!!1) . words) a ++ unbold
  | "Closure" `isPrefixOf` a = bold ++ "<<closure>>" ++ unbold
  | "List" `isPrefixOf` a    = a
--  | "List" `isPrefixOf` a    = "[" ++ bold ++ intercalate ", " (map ppLit (pprList a)) ++ unbold ++ "]"
  | "LInt" `isInfixOf` a     = bold ++ noParens ((init . (!!2). words) a) ++ unbold
  | "LDouble" `isInfixOf` a  = bold ++ noParens ((init . (!!2) . words) a) ++ unbold
  | "LBoolean" `isInfixOf` a = bold ++ (init . (!!2) . words) a ++ unbold
  | "LChar" `isInfixOf` a    = bold ++ (init . (!!2) . words) a ++ unbold
  | "LString" `isInfixOf` a  = bold ++ (init . unwords . drop 2 $ words a) ++ unbold
  | otherwise                = a

bold, unbold :: String
bold = "\ESC[37m"
unbold = "\ESC[0m"

pprList :: String -> [String]
pprList a = map L.unpack $ L.splitOn (L.pack ",") $ 
    L.pack $ filter (\x -> x `notElem` ['[', ']']) $ 
        unwords . tail . words $ a

noParens :: String -> String
noParens = filter (\x -> x `notElem` ['(', ')'])
