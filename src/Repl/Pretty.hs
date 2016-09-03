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
import Data.List (intercalate, isInfixOf)
import Data.List.Split (splitOn)
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
    ppr p (TArrow a b) = parensIf (isArrow a) (ppr p a) <+> text pbold <+> "→" <+> text clear <+> ppr p b  
        where
          isArrow TArrow{} = True
          isArrow _ = False
    ppr p (TVar a) = ppr p a
    ppr _ (TCon "Number") = text $ "\ESC[31m" ++ "Number" ++ clear
    ppr _ (TCon "String") = text $ "\ESC[32m" ++ "String" ++ clear
    ppr _ (TCon "Boolean") = text $ "\ESC[33m" ++ "Boolean" ++ clear
    ppr _ (TCon "Char") = text $ "\ESC[34m" ++ "Char" ++ clear
    ppr _ (TCon (x:xs))
              | x == '[' = text $ "[" ++ concatMap pprLit (splitOn "," $ init xs) ++ "]"
              | x == '{' = text $ "{" ++ intercalate ", " (map pprLit (splitOn "," $ init xs)) ++ "}"
              | otherwise = text (x:xs)

instance Pretty TypeScheme where
    ppr p (Forall [] t) = ppr p t
    ppr p (Forall ts t) = text "for all" <+> hcat (punctuate space (map (ppr p) ts)) <> text "." <+> ppr p t

instance Pretty Expr where
    ppr _ Closure{}              = text "<<closure>>"
    ppr _ (Lit (LInt i))         = integer i
    ppr _ (Lit (LDouble d))      = double d
    ppr _ (Lit (LString str))    = doubleQuotes $ text str 
    ppr _ (Lit (LChar c))        = quotes $ text [c]        -- convert char to a string
    ppr _ (Lit (LBoolean True))  = text "true"
    ppr _ (Lit (LBoolean False)) = text "false"
    ppr _ Nil                    = text "empty list"
    ppr _ ls@(BinOp OpCons _ _)  = text $ matchBrackets (ppList ls)
    ppr _ (Lit (LTup xs))        = text $ "{" ++ intercalate ", " (map ppexpr xs) ++ "}"
    ppr _ xs                     = text $ show xs

instance Show TypeError where
      show (UnificationFail a b) =
        concat ["Cannot ", "\ESC[33m", "match", clear, " expected type ", bold, pptype a, clear, " with actual type ", bold, pptype b, clear]
      show (InfiniteType (TV a) b) =
        concat ["Cannot construct the ", red, "infinite type", clear, ": ", a, " = ", pptype b]
      show (Ambigious cs) =
        concat ["Cannot not match expected type: '" ++ pptype a ++ "' with actual type: '" ++ pptype b ++ "'\n" | (a,b) <- cs]
      show (UnboundVariable a) = "Not in scope: " ++  bold ++ a ++ clear
      show (UnsupportedOperation a) = bold ++ a ++ clear
      show (UnificationMismatch a b) = show a ++ show b
      show (BadArg a s) = ppexpr a ++ s

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
      tup@(Lit (LTup _))   -> ppexpr tup
      Lit (LBoolean True)  -> "true"
      Lit (LBoolean False) -> "false"
ppList x                 = show x

matchBrackets :: String -> String
matchBrackets st = let len = length . takeWhile (== ']') . reverse $ st
                  in replicate len '[' ++ st

-- | more nasty kludge code here :(
pprLit :: String -> String
pprLit xs 
    | "LInt" `isInfixOf` xs = "\ESC[31m" ++ "Number" ++ clear
    | "LDouble" `isInfixOf` xs = "Number"
    | "LChar" `isInfixOf` xs = "Char"
    | "LString" `isInfixOf` xs = "String"
    | "LBoolean" `isInfixOf` xs = "Boolean"
    | otherwise = xs

ppexpr :: Expr -> String
ppexpr = render . ppr 0

ppsig :: (Expr, TypeScheme) -> String
ppsig (a, b) = pbold ++ ppexpr a ++ clear ++ " : " ++ ppscheme b

ppsig' :: (String, TypeScheme) -> String
ppsig' (a, b) = pbold ++ a ++ clear ++ " : " ++ ppscheme b

ppenv :: Env -> [String]
ppenv (TypeEnv env) = map ppsig' $ Map.toList env

pbold :: String
pbold = "\ESC[37m"
