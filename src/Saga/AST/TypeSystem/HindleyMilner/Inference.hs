{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Saga.AST.TypeSystem.HindleyMilner.Inference where

import           Control.Monad.Except
import           Control.Monad.State.Lazy                      (MonadState,
                                                                State,
                                                                evalState,
                                                                evalStateT,
                                                                replicateM)
import           Control.Monad.Trans.Except                    (ExceptT,
                                                                runExceptT)
import           Data.Bifunctor                                (Bifunctor (first))
import           Data.Functor                                  ((<&>))
import           Data.List                                     (nub)
import qualified Data.Map                                      as Map
import qualified Data.Set                                      as Set
import           Debug.Trace                                   (trace, traceM)
import           Saga.AST.TypeSystem.HindleyMilner.Environment
import           Saga.AST.TypeSystem.HindleyMilner.Types
import           Saga.AST.TypeSystem.HindleyMilner.Types       (Type (TArrow, TRecord, TTuple),
                                                                TypeExpr (TEArrow, TERecord, TETuple))
import           Saga.Parser.ParserHM                          (runSagaExpr)
import           Saga.Parser.ParsingInfo

-- import           Control.Monad.State.Lazy
-- import           Control.Monad.Trans.Except    (ExceptT)
-- import qualified Data.Map                      as Map
-- import qualified Data.Set                      as Set
-- import           Saga.AST.Syntax               (Expr, Name (..))
-- import           Saga.AST.TypeSystem.Inference hiding (Infer, TypeError, fresh,
--                                                 letters)
-- import           Saga.AST.TypeSystem.Types

run :: String -> Either String Scheme
run input = do
  Parsed expr _ _ <- runSagaExpr input
  show `first` runInfer (infer expr)

runInfer :: Infer (Subst, Type) -> Either TypeError Scheme
runInfer m = runExcept $ evalStateT inference [empty]
  where
    inference = do
      res <- m
      scheme <- closeOver res
      normalize scheme

closeOver :: (Map.Map TVar Type, Type) -> Infer Scheme
closeOver (sub, ty) = do
  scheme <- generalize $ apply sub ty
  normalize scheme

lookupVar :: TVar -> Infer (Subst, Type)
lookupVar v = do
  (Env vars _ _) <- get
  case Map.lookup v vars of
    Nothing -> throwError $ UnboundVariable v
    Just s -> do
      t <- instantiate s
      return (nullSubst, t)

instantiate :: Scheme -> Infer Type
instantiate (Scheme as t) = do
  as' <- mapM (const fresh) as
  let s = Map.fromList $ zip as as'
  return $ apply s t

extend :: TypeEnv -> (TVar, Scheme) -> TypeEnv
extend e@(Env vars aliases count) (var, scheme) = e {typeVars = Map.insert var scheme vars}

class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set.Set TVar

instance (Substitutable a) => Substitutable [a] where
  apply = map . apply
  ftv = foldl union Set.empty
    where
      union set x = Set.union (ftv x) set

instance Substitutable Type where
  apply s t | trace ("Applying type sub: " ++ show s ++ " to " ++ show t) False = undefined
  apply s t@(TVar id) = Map.findWithDefault t id s
  apply s (inTy `TArrow` outTy) = in' `TArrow` out'
    where
      in' = apply s inTy
      out' = apply s outTy
  apply _ ty = ty

  ftv (TVar id) = Set.singleton id
  ftv (t `TArrow` t') = set `Set.union` set'
    where
      set = ftv t
      set' = ftv t'
  ftv _ = Set.empty

instance Substitutable Scheme where
  apply s t | trace ("Applying scheme sub: " ++ show s ++ " to " ++ show t) False = undefined
  apply s (Scheme as t) = Scheme as ty
    where
      s' = foldr Map.delete s as
      ty = apply s' t

  ftv (Scheme as t) = set `Set.difference` Set.fromList as
    where
      set = ftv t

instance Substitutable TypeEnv where
  apply s t | trace ("Applying type env sub: " ++ show s ++ " to " ++ show (typeVars t)) False = undefined
  apply s e@(Env vars aliases count) = e {typeVars = Map.map (apply s) vars}
  ftv (Env vars aliases count) = ftv $ Map.elems vars

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = s `Map.union` s1
  where
    s = Map.map (apply s1) s2

unify :: Type -> Type -> Infer Subst
unify t1 t2 | trace ("Unifying: " ++ show t1 ++ " with " ++ show t2) False = undefined
unify (il `TArrow` ol) (ir `TArrow` or) = do
  s <- unify il ir
  let t = apply s ol
  let t' = apply s or
  s' <- unify t t'

  return $ s' `compose` s
unify (TVar a) t = bind a t
unify t (TVar a) = bind a t
unify (TPrimitive a) (TPrimitive b) | a == b = return nullSubst
unify (TLiteral a) (TLiteral b) | a == b = return nullSubst
unify (TTuple as) (TTuple bs) = do
  ss <- zipWithM unify as bs
  return $ foldl compose nullSubst ss
unify sub@(TRecord as) parent@(TRecord bs) = sub `isSubtype` parent
unify (TConstrained _ ty) ty' = unify ty ty'
unify ty (TConstrained _ ty') = unify ty ty'
unify t t' = throwError $ UnificationFail t t'

bind :: TVar -> Type -> Infer Subst
bind a t | trace ("Binding: " ++ show a ++ " to " ++ show t) False = undefined
bind a t
  | t == TVar a = return nullSubst
  | occursCheck a t = throwError $ InfiniteType a t
  | otherwise = return $ Map.singleton a t

occursCheck :: Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` set
  where
    set = ftv t

generalize :: Type -> Infer Scheme
generalize t | trace ("Generalizing: " ++ show t) False = undefined
generalize t = do
  env <- get
  let as = Set.toList $ ftv t `Set.difference` ftv env
  return $ Scheme as t


lookupEnv :: TVar -> Infer (Subst, Type)
lookupEnv x = do
  (Env vars aliases count) <- get
  case Map.lookup x aliases of
    Just ty -> return (nullSubst, ty)
    Nothing -> case Map.lookup x vars of
      Nothing -> throwError $ UnboundVariable (show x)
      Just s -> do
        t <- instantiate s
        return (nullSubst, t)

infer :: Expr -> Infer (Subst, Type)
infer ex | trace ("Inferring: " ++ show ex) False = undefined
infer ex = case ex of
  Identifier x -> lookupEnv x
  Lambda (param : rest) body -> do
    tvar <- fresh
    modify $ \env -> env `extend` (param, Scheme [] tvar)
    (s, t) <- infer $ case rest of
      [] -> body
      _  -> Lambda rest body
    let t' = apply s tvar
    return (s, t' `TArrow` t) -- TO
  FnApp fn [arg] -> do
    tv <- fresh
    (fnSub, fnTy) <- infer fn
    (argSub, argTy) <- infer arg
    let ty = apply argSub fnTy
    unifier <- unify ty (argTy `TArrow` tv)
    let sub = unifier `compose` argSub `compose` fnSub
    let ty' = apply unifier tv
    return (sub, ty')
  FnApp fn (a : as) -> infer curried
    where
      partial = FnApp fn [a]
      curried = foldl (\f a -> FnApp f [a]) partial as
  Assign x e -> do
    (s, t) <- infer e
    t' <- generalize t
    modify $ \env -> env `extend` (x, t')
    return (s, t)
  IfElse cond tr fl -> do
    tv <- fresh
    let tArrow t t' = t `TArrow` t'
    inferPrim [cond, tr, fl] $ TPrimitive TBool `tArrow` tv `tArrow` tv `tArrow` tv

  Tuple elems -> do
    tElems <- mapM infer' elems
    return (nullSubst, TTuple tElems)
    where
      infer' = extract . infer
      extract = fmap snd
  Record pairs -> do
    tPairs <- mapM infer' pairs
    return (nullSubst, TRecord tPairs)
    where
      infer' = mapM $ extract . infer
      extract = fmap snd
  Term (LInt i) -> return (nullSubst, TLiteral (LInt i))
  Term (LBool b) -> return (nullSubst, TLiteral (LBool b))
  Term (LString s) -> return (nullSubst, TLiteral (LString s))

inferPrim :: [Expr] -> Type -> Infer (Subst, Type)
inferPrim l t = do
  tv <- fresh
  (s, tf) <- foldM inferStep (nullSubst, id) l
  let t' = apply s (tf tv)
  s' <- unify t' t

  return (s' `compose` s, apply s' tv)
  where
    inferStep (s, tf) exp = do
      -- modifyM $ apply s
      (s', t) <- infer exp
      return (s' `compose` s, tf . TArrow t)

inferExpr :: Expr -> Either TypeError Scheme
inferExpr = runInfer . infer


normalize :: Scheme -> Infer Scheme
normalize sc | trace ("Normalizing: " ++ show sc) False = undefined
normalize (Scheme ts body) = do
  body' <- fv body
  let ord = zip (nub body') letters
  ty <- normtype body ord
  return $ Scheme (fmap snd ord) ty
  where
    fv (TVar a) = return [a]
    fv (TArrow a b) = do
      fva <- fv a
      fvb <- fv b
      return $ fva ++ fvb
    fv _ = return []

    normtype (TArrow a b) ord = do
      ta <- normtype a ord
      tb <- normtype b ord
      return $ TArrow ta tb
    normtype (TVar a) ord = return $
      case lookup a ord of
        Just x  -> TVar x
        Nothing -> error "type variable not in signature"
    normtype ty _ = return ty

isSubtype :: Type -> Type -> Infer Subst
-- isSubtype  a b | trace ("subtype " ++ show a ++ " <: " ++ show b ++ "\n  ") False = undefined
TLiteral (LInt _) `isSubtype` TPrimitive TInt = return nullSubst
TLiteral (LString _) `isSubtype` TPrimitive TString = return nullSubst
TLiteral (LBool _) `isSubtype` TPrimitive TBool = return nullSubst
TPrimitive prim1 `isSubtype` TPrimitive prim2 | prim1 == prim2 = return nullSubst
sub@(TRecord pairs1) `isSubtype` parent@(TRecord pairs2) = do
  let check (name, ty2) = case lookup name pairs1 of
        Nothing  -> throwError $ UnificationFail sub parent
        Just ty1 -> unify ty1 ty2

  subs <- mapM check pairs2
  return $ foldl compose nullSubst subs
sub `isSubtype` parent = throwError $ SubtypeFailure sub parent
