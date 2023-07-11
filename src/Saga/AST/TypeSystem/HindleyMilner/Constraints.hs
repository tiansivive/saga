

module Saga.AST.TypeSystem.HindleyMilner.Constraints where

import           Control.Monad.Except
import           Control.Monad.State                           (StateT (runStateT),
                                                                evalStateT, get,
                                                                modify, put)
import           Saga.AST.TypeSystem.HindleyMilner.Environment

import           Data.Bifunctor                                (bimap)
import           Data.List                                     (delete)
import qualified Data.Map                                      as Map
import qualified Data.Set                                      as Set
import           Debug.Trace
import           Saga.AST.TypeSystem.HindleyMilner.Types
import           Text.Pretty.Simple                            (pShow)


type Solve = StateT Unifier (Except InferenceError)

type Subst = Map.Map UnificationVar Type

type Unifier = (Subst, [IConstraint])


class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set.Set UnificationVar

instance (Substitutable a) => Substitutable [a] where
  apply = map . apply

  ftv = foldl union Set.empty
    where
      union set x = Set.union (ftv x) set

instance Substitutable Type where
  apply s t | trace ("Applying type sub\n\t" ++ show s ++ "\n\t" ++ show t) False = undefined
  apply s t@(TVar id) = Map.findWithDefault t id s
  apply s (inTy `TArrow` outTy) = in' `TArrow` out'
    where
      in' = apply s inTy
      out' = apply s outTy
  apply s (TConstrained cs ty) = apply s ty
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
  apply s t | trace ("Applying type env sub\n\t" ++ show s ++ "\n\t" ++ show (unifier t)) False = undefined
  apply s e@(Env vars aliases) = e {unifier = Map.map (apply s) vars}
  ftv (Env vars aliases ) = ftv $ Map.elems vars

instance Substitutable IConstraint where
   apply s (t1 `Equals` t2) = apply s t1 `Equals` apply s t2
   apply s c                = c
   ftv (t1 `Equals` t2) = ftv t1 `Set.union` ftv t2


runSolve :: [IConstraint] -> Either InferenceError Subst
runSolve cs | trace ("Solving: " ++ show cs ) False = undefined
runSolve cs = runExcept $ evalStateT solver st
  where st = (nullSubst, cs)

solver :: Solve Subst
solver = do
  (sub, constraints) <- get
  traceM $ "-------------------\nSolve state" ++ "\n\tsubs: " ++ show sub ++ "\n\tConstraints: " ++ show constraints ++ "\n"
  case constraints of
    [] -> return sub
    (c: cs) -> case c of
        Equals t1 t2 -> do
            sub'  <- unify t1 t2
            modify $ bimap (compose sub') (apply sub' . delete c)
            solver
        _ -> do -- throwError $ Fail "Not implemented yet"
            modify $ fmap (delete c)
            solver

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = s `Map.union` s1
  where
    s = Map.map (apply s1) s2

unify :: Type -> Type -> Solve Subst
unify t1 t2 | trace ("Unifying:\n\t" ++ show t1 ++ "\n\t" ++ show t2) False = undefined
unify (il `TArrow` ol) (ir `TArrow` or) = do
  sub <- unify il ir
  s <- apply sub ol `unify` apply sub or
  return $ s `compose` sub
unify (TVar a) t = bind a t
unify t (TVar a) = bind a t
unify (TPrimitive a) (TPrimitive b) | a == b = return nullSubst
unify (TLiteral a) (TLiteral b) | a == b = return nullSubst
unify (TTuple as) (TTuple bs) = do
  ss <- zipWithM unify as bs
  return $ foldl compose nullSubst ss
unify sub@(TRecord as) parent@(TRecord bs) = sub `isSubtype` parent
unify (TConstrained constraints ty) ty' = do
    modify $ \(sub, cs) -> (sub, cs ++ fmap protocol constraints)
    unify ty ty'
    where
        protocol (t `Implements` protocol) = Protocol protocol [t]
unify ty (TConstrained constraints ty') = do
    modify $ \(sub, cs) -> (sub, cs ++ fmap protocol constraints)
    unify ty ty'
    where
        protocol (t `Implements` protocol) = Protocol protocol [t]
unify t t' = throwError $ UnificationFail t t'


bind :: UnificationVar -> Type -> Solve Subst
bind a t | trace ("Binding: " ++ a ++ " to " ++ show t) False = undefined
bind a t
  | t == TVar a = return nullSubst
  | occursCheck a t = throwError $ InfiniteType a t
  | TLiteral l <- t = return . Map.singleton a $
    case l of
        LInt _    ->  TPrimitive TInt
        LString _ -> TPrimitive TString
        LBool _   -> TPrimitive TBool
  | otherwise = return $ Map.singleton a t

occursCheck :: Substitutable a => UnificationVar -> a -> Bool
occursCheck a t = a `Set.member` set
  where
    set = ftv t


nullSubst :: Subst
nullSubst = Map.empty



isSubtype :: Type -> Type -> Solve Subst
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


