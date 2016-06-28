{-# Language FlexibleInstances #-}
{-# Language TypeSynonymInstances #-}

module Repl.Pretty  where

import Language.Typing.Type
import Language.Syntax
--import Language.Typing.Env
import Language.Typing.TypeError

import Text.PrettyPrint
--import qualified Data.Map as Map

ppsig :: (String, TypeScheme) -> String
ppsig (a, b) = a ++ " :: " ++ ppscheme b

ppscheme :: TypeScheme -> String
ppscheme = render . ppr 0

ppTypeError :: TypeError -> String
ppTypeError = render . ppError

{-ppenv :: Env -> [String]-}
{-ppenv env = map ppsig $ Map.toList env-}

ifParens True = parens
ifParens False = id

class Pretty p where
    ppr :: Int -> p -> Doc

instance Pretty Name where
    ppr _  = text 

instance Pretty TVar where
    ppr _ (TV x) = text x

instance Pretty Type where
    ppr p (TArr a b) = ifParens (isArrow a) (ppr p a) <+> text "->" <+> ppr p b
        where 
            isArrow TArr{} = True
            isArrow _      = False
    ppr p (TVar a) = ppr p a
    ppr _ (TCon a) = text a
    
instance Pretty TypeScheme where
    ppr p (Forall [] t) = ppr p t
    ppr p (Forall ts t) = text "forall" <+> hcat (punctuate space (map (ppr p) ts)) <> text "." <+> ppr p t
