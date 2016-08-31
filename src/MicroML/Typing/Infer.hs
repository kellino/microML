{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module MicroML.Typing.Infer (
  inferTop,
  constraintsExpr,
) where

import MicroML.Typing.Env
import MicroML.Typing.Substitutable
import MicroML.Typing.TypeError
import qualified MicroML.Typing.Env as Env
import MicroML.Typing.Type
import MicroML.Syntax

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.RWS
import Control.Monad.Identity

import Data.List (nub) --, isInfixOf)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Inference monad
type Infer a = (RWST Env [Constraint] InferState (Except TypeError) a)  

-- | Inference state
data InferState = InferState { count :: Int }

-- | Initial inference state
initInfer :: InferState
initInfer = InferState { count = 0 }

-------------------------------------------------------------------------------
-- Inference
-------------------------------------------------------------------------------

-- | Run the inference monad
runInfer :: Env -> Infer Type -> Either TypeError (Type, [Constraint])
runInfer env m = runExcept $ evalRWST m env initInfer

-- | Solve for the toplevel type of an expression in a given environment
inferExpr :: Env -> Expr -> Either TypeError TypeScheme
inferExpr env ex = case runInfer env (infer ex) of
  Left err -> Left err
  Right (ty, cs) -> case runSolve cs of
    Left err -> Left err
    Right subst -> Right $ closeOver $ apply subst ty

-- | Return the internal constraints used in solving for the type of an expression
constraintsExpr :: Env -> Expr -> Either TypeError ([Constraint], Subst, Type, TypeScheme)
constraintsExpr env ex = case runInfer env (infer ex) of
  Left err -> Left err
  Right (ty, cs) -> case runSolve cs of
    Left err -> Left err
    Right subst -> Right (cs, subst, ty, sc)
      where
        sc = closeOver $ apply subst ty

-- | Canonicalize and return the polymorphic toplevel type.
closeOver :: Type -> TypeScheme
closeOver = normalize . generalize Env.empty

-- | Unify two types
uni :: Type -> Type -> Infer ()
uni t1 t2 = tell [(t1, t2)]

-- | Extend type environment
inEnv :: (Name, TypeScheme) -> Infer a -> Infer a
inEnv (x, sc) m = do
  let scope e = restrict e x `extend` (x, sc)
  local scope m

-- | Lookup type in the environment
lookupEnv :: Name -> Infer Type
lookupEnv x = do
  (TypeEnv env) <- ask
  case Map.lookup x env of
      Nothing   ->  throwError $ UnboundVariable $ show x
      Just s    ->  instantiate s

-- letters and fresh generate new names for polymorphic variables
letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: Infer Type
fresh = do
    s <- get
    put s{count = count s + 1}
    return $ TVar $ TV (letters !! count s)

instantiate ::  TypeScheme -> Infer Type
instantiate (Forall as t) = do
    as' <- mapM (const fresh) as
    let s = Subst $ Map.fromList $ zip as as'
    return $ apply s t

generalize :: Env -> Type -> TypeScheme
generalize env t  = Forall as t
    where as = Set.toList $ ftv t `Set.difference` ftv env

---------------------------
-- OVERLOADED OPERATIONS --
---------------------------

mathsOps :: Map.Map Binop Type
mathsOps = Map.fromList [
         ( OpAdd,    typeNum `TArrow`  ( typeNum `TArrow`  typeNum))
      ,  ( OpMul,    typeNum `TArrow`  ( typeNum `TArrow`  typeNum))
      ,  ( OpSub,    typeNum `TArrow`  ( typeNum `TArrow`  typeNum))
      ,  ( OpMod,    typeNum `TArrow`  ( typeNum `TArrow`  typeNum))
      ,  ( OpDiv,    typeNum `TArrow`  ( typeNum `TArrow`  typeNum))
      ,  ( OpIntDiv, typeNum `TArrow`  ( typeNum `TArrow`  typeNum))
      ,  ( OpExp,    typeNum `TArrow`  ( typeNum `TArrow`  typeNum))
      ,  ( OpGe,     typeNum `TArrow`  ( typeNum `TArrow`  typeBool))
      ,  ( OpLe,     typeNum `TArrow`  ( typeNum `TArrow`  typeBool))
      ,  ( OpGt,     typeNum `TArrow`  ( typeNum `TArrow`  typeBool))
      ,  ( OpLt,     typeNum `TArrow`  ( typeNum `TArrow`  typeBool))
      ,  ( OpEq,     typeNum `TArrow`  ( typeNum `TArrow`  typeBool))
      ,  ( OpNotEq,  typeNum `TArrow`  ( typeNum `TArrow`  typeBool))
  ]

charOps :: Map.Map Binop Type
charOps = Map.fromList [
        ( OpEq, typeChar `TArrow` (typeChar `TArrow` typeBool))                       
      , ( OpLe, typeChar `TArrow` (typeChar `TArrow` typeBool))
      , ( OpLt, typeChar `TArrow` (typeChar `TArrow` typeBool))
      , ( OpGe, typeChar `TArrow` (typeChar `TArrow` typeBool))
      , ( OpGt, typeChar `TArrow` (typeChar `TArrow` typeBool))
      , ( OpNotEq, typeChar `TArrow` (typeChar `TArrow` typeBool))
  ]

boolOps :: Map.Map Binop Type
boolOps = Map.fromList [
        ( OpEq,    typeBool `TArrow` ( typeBool `TArrow` typeBool ))
      , ( OpNotEq, typeBool `TArrow` ( typeBool `TArrow` typeBool ))
      , ( OpOr, typeBool `TArrow` ( typeBool `TArrow` typeBool ))
      , ( OpAnd, typeBool `TArrow` ( typeBool `TArrow` typeBool ))
      , ( OpXor,   typeBool `TArrow` ( typeBool `TArrow` typeBool ))
  ]

---------------
-- INFERENCE --
---------------

inferTop :: Env -> [(Name, Expr)] -> Either TypeError Env
inferTop env [] = Right env
inferTop env ((name, ex):xs) = case inferExpr env ex of
  Left err -> Left err
  Right ty -> inferTop (extend env (name, ty)) xs

normalize :: TypeScheme -> TypeScheme
normalize (Forall _ body) = Forall (map snd ord) (normtype body)
  where
    ord = zip (nub $ fv body) (map TV letters)

    fv (TVar a)   = [a]
    fv (TArrow a b) = fv a ++ fv b
    fv (TCon _)    = []

    normtype (TArrow a b) = TArrow (normtype a) (normtype b)
    normtype (TCon a)   = TCon a
    normtype (TVar a)   =
      case Prelude.lookup a ord of
        Just x -> TVar x
        Nothing -> error "type variable not in signature"

-----------------------------
-- MAIN INFERENCE FUNCTION --
-----------------------------

infer :: Expr -> Infer Type
infer expr = case expr of
    Lit (LInt _)     -> return typeNum
    Lit (LDouble _)  -> return typeNum
    Lit (LBoolean _) -> return typeBool
    Lit (LString _)  -> return typeString
    Lit (LChar _)    -> return typeChar
    PrimitiveErr _   -> return typeError
    Nil              -> return $ TVar $ TV "[a]"

    -- work in progress. This is just a placeholder
    Lit (LTup (x:xs)) -> do
        t1 <- infer x
        tv <- fresh
        uni t1 tv
        infer $ Lit . LTup $ xs

    Var x -> lookupEnv x

    Lam x e -> do
       tv <- fresh
       t <- inEnv (x, Forall [] tv) (infer e)
       return (tv `TArrow` t)
             
    App e1 e2 -> do
        t1 <- infer e1
        t2 <- infer e2
        tv <- fresh
        uni t1 (t2 `TArrow` tv)
        return tv

    Let x e1 e2 -> do
        env <- ask
        t1 <- infer e1
        let sc = generalize env t1
        inEnv (x, sc) (infer e2)

    FixPoint e1 -> do
        t1 <- infer e1
        tv <- fresh
        uni (tv `TArrow` tv) t1
        return tv

    UnaryOp op e1 -> 
        case op of
          Car -> 
              case e1 of
                BinOp OpCons x _  -> infer x
                Nil               -> throwError $ UnsupportedOperation "head of an empty list"
                Var _             -> do
                    TVar (TV tv) <- fresh
                    return $ TVar $ TV $ "[" ++ tv ++ "]"
                x                 -> throwError $ UnsupportedOperation $ "car: " ++ show x
          Cdr -> infer e1
          Show -> return typeString
          Read -> return typeNum
          OpLog -> infer e1 
          Chr -> return typeChar
          Ord -> return typeNum
          Minus -> do
              t1 <- infer e1
              uni t1 typeNum
              return t1

    BinOp op e1 e2 -> 
        case op of
          OpCons  -> doConsOp e1 e2
          OpGt    -> inferBinOpBool e1 e2
          OpGe    -> inferBinOpBool e1 e2
          OpLt    -> inferBinOpBool e1 e2
          OpEq    -> inferBinOpBool e1 e2
          OpNotEq -> inferBinOpBool e1 e2
          OpLe    -> inferBinOpBool e1 e2
          OpPipe  -> infer e2
          _       -> 
              case (e1, e2) of
                  nil@(_, Nil)         -> throwError $ UnsupportedOperation $ "[] error " ++ show nil -- debugging
                  (Lit (LInt _),_)     -> doBinaryMathsOp op e1 e2
                  (Lit (LDouble _),_)  -> doBinaryMathsOp op e1 e2
                  (Lit (LBoolean _),_) -> doBinaryBoolOp op e1 e2
                  (Lit (LChar _),_)    -> doBinaryCharOp op e1 e2
                  (Lit (LString _),_)  -> do
                      t1 <- infer e1
                      t2 <- infer e2
                      uni t1 t2
                      return t2
                  (var@(Var _), _)     -> infer var
                  (op'@UnaryOp{}, _)   -> infer op'
                  (op'@BinOp{}, _)     -> infer op'
                  (app@App{},_)        -> infer app
                  _                    -> throwError $ UnsupportedOperation $ "the " ++ show op ++ " operation is not supported on " ++ show e1 ++ show e2

    If cond tr fl -> do
        t1 <- infer cond
        t2 <- infer tr
        t3 <- infer fl
        uni t1 typeBool
        uni t2 t3
        return t3

    -- should never actually reach this, but discretion is the better part of valour
    x -> throwError $ UnsupportedOperation $ "general error: " ++ show x

-------------------------------
-- UNARY & BINARY OPERATIONS --
-------------------------------

inferBinOpBool :: Expr -> Expr -> Infer Type
inferBinOpBool e1 e2 = do
    t1 <- infer e1
    t2 <- infer e2
    uni t1 t2
    return typeBool

doConsOp :: Expr -> Expr -> Infer Type
doConsOp e1 e2 = 
     case (e1, e2) of 
          (App x y , _) -> do
              t1 <- infer x
              t2 <- infer y
              uni t1 t2
              return t1
          (_, Nil) -> do
              t1 <- infer e1
              return $ 
                  case t1 of
                    (TCon ty) -> TCon $ "[" ++ ty ++ "]"
                    _         -> TCon $ "[" ++ show e1 ++ "]"
          (_, BinOp OpCons x xs) -> do
              t1 <- infer e1
              t2 <- infer x
              uni t1 t2
              doConsOp x xs
          (Var _, Var _) -> do
              ty1@(TCon t1) <- infer e1
              t2 <- infer e2
              uni (TCon $ "[" ++ t1 ++ "]") t2
              return ty1 
          (_, Var _) -> unifyWithListVar e1 e2
          (UnaryOp Car x, _) -> infer x
          (UnaryOp Cdr x, _) -> infer x
          _     -> do -- UnificationFail (infer e1) (infer e2)
              t1 <- infer e1
              t2 <- infer e2
              throwError $ UnificationFail t1 t2

unifyWithListVar :: Expr -> Expr -> Infer Type
unifyWithListVar e1 e2 =
    case (e1, e2) of
      (Lit{} , _) -> newListTypeCon e1
      (_, _) -> do
          t1 <- infer e1
          t2 <- infer e2
          throwError $ UnificationFail t1 t2

newListTypeCon :: Expr -> Infer Type
newListTypeCon e1 = do
    (TCon ty) <- infer e1
    return $ TCon $ "[" ++ ty ++ "]"

doBinaryMathsOp :: Binop -> Expr -> Expr -> Infer Type
doBinaryMathsOp op e1 e2 = do
    t1 <- infer e1
    t2 <- infer e2
    case op of 
      OpAdd    -> getOp mathsOps op t1 t2
      OpSub    -> getOp mathsOps op t1 t2
      OpMul    -> getOp mathsOps op t1 t2
      OpIntDiv -> getOp mathsOps op t1 t2
      OpExp    -> getOp mathsOps op t1 t2
      OpEq     -> getOp mathsOps op t1 t2
      OpLe     -> getOp mathsOps op t1 t2
      OpLt     -> getOp mathsOps op t1 t2
      OpGe     -> getOp mathsOps op t1 t2
      OpGt     -> getOp mathsOps op t1 t2
      OpNotEq  -> getOp mathsOps op t1 t2
      OpPipe   -> return typeNum
      OpMod    ->
          case (e1, e2) of
            (Lit (LInt _), Lit (LInt _)) -> getOp mathsOps OpMod t1 t2
            (Lit (LDouble d), _)  -> throwError $ UnsupportedOperation $ "both numbers must be integers, whereas " ++ show d ++ " is a floating point number"
            (_, Lit (LDouble d))  -> throwError $ UnsupportedOperation $ "both numbers must be integers, whereas " ++ show d ++ " is a floating point number"
      OpDiv   -> 
          case e2 of
            (Lit (LInt 0)) -> throwError $ UnsupportedOperation "you cannot divide by 0"
            (Lit (LDouble 0.0)) -> throwError $ UnsupportedOperation "you cannot divide by 0"
            _              -> getOp mathsOps OpDiv t1 t2
      _       -> throwError $ UnsupportedOperation $ "you cannot do " ++ show op ++ "\ESC[1m" ++ " with numbers" ++ "\ESC[0m"

doBinaryCharOp :: Binop -> Expr -> Expr -> Infer Type
doBinaryCharOp op e1 e2 = do
    t1 <- infer e1
    t2 <- infer e2
    case op of
      OpEq    -> getOp charOps OpEq t1 t2
      OpLe    -> getOp charOps OpLe t1 t2
      OpLt    -> getOp charOps OpLt t1 t2
      OpGe    -> getOp charOps OpGe t1 t2
      OpGt    -> getOp charOps OpGt t1 t2
      OpNotEq -> getOp charOps OpNotEq t1 t2
      _       -> throwError $ UnsupportedOperation $ "you cannot do " ++ show op ++ "\ESC[1m" ++ " with chars" ++ "\ESC[0m"

doBinaryBoolOp :: Binop -> Expr -> Expr -> Infer Type
doBinaryBoolOp op e1 e2 = do
    t1 <- infer e1
    t2 <- infer e2
    case op of
      OpEq    -> getOp boolOps OpEq t1 t2
      OpNotEq -> getOp boolOps OpNotEq t1 t2
      OpAnd   -> getOp boolOps OpAnd t1 t2
      OpOr    -> getOp boolOps OpOr t1 t2
      OpXor   -> getOp boolOps OpXor t1 t2
      _       -> throwError $ UnsupportedOperation $ "you can't do " ++ show op ++ " \ESC[1m" ++ "with booleans" ++ "\ESC[0m"

getOp :: (Ord k) => Map.Map k Type -> k -> Type -> Type -> Infer Type
getOp dict op t1 t2 = do
    tv <- fresh
    let u1 = t1 `TArrow` (t2 `TArrow` tv)
        u2 = dict Map.! op
    uni u1 u2
    return tv

-------------------------------------------------------------------------------
-- Constraint Solver
-------------------------------------------------------------------------------

-- | The empty substitution
emptySubst :: Subst
emptySubst = mempty

-- | Compose substitutions
compose :: Subst -> Subst -> Subst
(Subst s1) `compose` (Subst s2) = Subst $ Map.map (apply (Subst s1)) s2 `Map.union` s1

-- | Run the constraint solver
runSolve :: [Constraint] -> Either TypeError Subst
runSolve cs = runIdentity $ runExceptT $ solver st
  where st = (emptySubst, cs)

unifyMany :: [Type] -> [Type] -> Solve Subst
unifyMany [] [] = return emptySubst
unifyMany (t1 : ts1) (t2 : ts2) =
  do su1 <- unifies t1 t2
     su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
     return (su2 `compose` su1)
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

unifies :: Type -> Type -> Solve Subst
unifies t1 t2 | t1 == t2 = return emptySubst
unifies (TVar v) t = v `bind` t
unifies t (TVar v) = v `bind` t
unifies (TArrow t1 t2) (TArrow t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies t1 t2 = throwError $ UnificationFail t1 t2

-- Unification solver
solver :: Unifier -> Solve Subst
solver (su, cs) =
  case cs of
    [] -> return su
    ((t1, t2): cs0) -> do
      su1  <- unifies t1 t2
      solver (su1 `compose` su, apply su1 cs0)

bind ::  TVar -> Type -> Solve Subst
bind a t | t == TVar a     = return emptySubst
         | occursCheck a t = throwError $ InfiniteType a t
         | otherwise       = return (Subst $ Map.singleton a t)

occursCheck ::  Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t
