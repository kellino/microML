{-# Language FlexibleInstances #-}
{-# Language TypeSynonymInstances #-}

module Repl.Pretty  where

import MicroML.Typing.Type
import MicroML.Syntax
import MicroML.Typing.TypeError

import Text.PrettyPrint

parensIf ::  Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

class Pretty p where
  ppr :: Int -> p -> Doc

instance Pretty Name where
    ppr _  = text 

instance Pretty TVar where
    ppr _ (TV x) = text $ "\ESC[1m" ++ x ++ "\ESC[0m"

instance Pretty Type where
  ppr p (TArr a b) = parensIf (isArrow a) (ppr p a) <+> text "->" <+> ppr p b
    where
      isArrow TArr{} = True
      isArrow _ = False
  ppr p (TVar a) = ppr p a
  ppr _ (TCon a) = text a

instance Pretty TypeScheme where
  ppr p (Forall [] t) = ppr p t
  ppr p (Forall ts t) = text "for all" <+> hcat (punctuate space (map (ppr p) ts)) <> text "." <+> ppr p t

instance Pretty Expr where
  ppr p (Var a) = ppr p a
  ppr p (App a b) = parensIf (p > 0) $ ppr (p+1) a <+> ppr p b
  ppr p (Lam a b) = text "\\" <> ppr p a <+> text  "->" <+> ppr p b
  ppr p (Let a b c) = text "let" <> ppr p a <+> text  "=" <+> ppr p b <+> text "in" <+> ppr p c
  ppr p (Lit a) = ppr p a
  ppr p (FixPoint a) = parensIf (p>0) $ text "fix" <> ppr p a
  ppr p (If a b c) =
    text "if" <> ppr p a <+>
    text "then" <+> ppr p b <+>
    text "else" <+> ppr p c

instance Pretty Lit where
  ppr _ (LInt i) = integer i
  ppr _ (LBoolean True) = text "True"
  ppr _ (LBoolean False) = text "False"

instance Show TypeError where
  show (UnificationFail a b) =
    concat ["Cannot \ESC[1munify\ESC[0m types: ", pptype a, " with ", pptype b]
  show (InfiniteType (TV a) b) =
    concat ["Cannot construct the \ESC[0minfinite\ESC[0m type: ", a, " = ", pptype b]
  show (Ambigious cs) =
    concat ["Cannot not match expected type: '" ++ pptype a ++ "' with actual type: '" ++ pptype b ++ "'\n" | (a,b) <- cs]
  show (UnboundVariable a) = "Not in scope: " ++ a

ppscheme :: TypeScheme -> String
ppscheme = render . ppr 0

pptype :: Type -> String
pptype = render . ppr 0

ppexpr :: Expr -> String
ppexpr = render . ppr 0

ppsig :: (String, TypeScheme) -> String
ppsig (a, b) = a ++ " \ESC[35m::\ESC[0m " ++ ppscheme b

ppdecl :: (String, Expr) -> String
ppdecl (a, b) = "let " ++ a ++ " = " ++ ppexpr b

ppTypeError :: TypeError -> String
ppTypeError = render . ppError
