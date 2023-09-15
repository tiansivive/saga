{-# LANGUAGE GADTs #-}
module Saga.Language.TypeSystem.HindleyMilner.Check where
import           Control.Monad.Except                               (Except,
                                                                     MonadError (throwError),
                                                                     runExcept)
import           Control.Monad.Reader                               (ReaderT (runReaderT))
import           Control.Monad.RWS                                  (RWST (runRWST),
                                                                     execRWST)
import qualified Data.Map                                           as Map
import           Saga.Language.TypeSystem.HindleyMilner.Constraints (HasKind (kind),
                                                                     ProtocolEnv,
                                                                     Solve,
                                                                     Subst,
                                                                     Substitutable (apply, ftv),
                                                                     compose,
                                                                     isSubtype,
                                                                     nullSubst,
                                                                     runSolve)

import           Control.Monad.Trans.RWS                            (evalRWST)
import           Data.Bifunctor                                     (first)
import           Debug.Trace                                        (trace,
                                                                     traceM)
import           Saga.Language.TypeSystem.HindleyMilner.Environment (IConstraint,
                                                                     Infer,
                                                                     InferenceEnv,
                                                                     InferenceError (..),
                                                                     InferenceState,
                                                                     Scheme)

import           Saga.Language.TypeSystem.HindleyMilner.Inference   (closeOver,
                                                                     infer,
                                                                     runInfer)
import           Saga.Language.TypeSystem.HindleyMilner.Types       (Kind (KType),
                                                                     PrimitiveType (..),
                                                                     Type (..),
                                                                     Tyvar (Tyvar))

import           Saga.Language.Core.Syntax
import           Saga.Language.TypeSystem.HindleyMilner.Shared

import           Control.Monad                                      (zipWithM)
import           Data.Either
import qualified Data.Set                                           as Set
import           Saga.Language.Core.Literals                        (Literal (..))
import qualified Saga.Language.TypeSystem.HindleyMilner.Refinement  as Refine





type Check = ReaderT ProtocolEnv (Except InferenceError)

-- data TypeCheckError
--     = TypeMismatch Type Type
--     | KindMismatch Tyvar Type
--     | InfiniteType Tyvar Type



-- run :: Expr -> Type -> Either String (Subst, [IConstraint])
-- run expr ty = show `first` runExcept (evalRWST (check expr ty) empty initState)



check :: Expr -> Type -> Either String (Subst, InferenceState, [IConstraint])
check expr ty = do
    tyExpr <- show `first` runInfer (infer expr)
    inferred <- Refine.run tyExpr
    show `first` runExcept (runRWST ( inferred `matches` (TPrimitive TInt `TArrow` TPrimitive TInt)) empty initState)
    -- inferred `matches` ty


-- | TODO: Need to have different unification rules here between type literals and primitives
matches :: (MonadError e m, e ~ InferenceError) => Type -> Type -> m Subst
--matches t1 t2 | trace ("Unifying:\n\t" ++ show t1 ++ "\n\t" ++ show t2) False = undefined

matches (TLiteral a) (TLiteral b) | a == b = return nullSubst
matches (TPrimitive a) (TPrimitive b) | a == b = return nullSubst
matches (TData lCons) (TData rCons) | lCons == rCons = return nullSubst
matches sub@(TRecord as) parent@(TRecord bs) = sub `isSubtype` parent
matches lit@(TLiteral _) prim@(TPrimitive _) = lit `isSubtype` prim
matches (TTuple as) (TTuple bs) = do
  ss <- zipWithM matches as bs
  return $ foldl compose nullSubst ss

matches (il `TArrow` ol) (ir `TArrow` or) = do
  sub <- matches il ir
  s <- apply sub ol `matches` apply sub or
  return $ s `compose` sub
matches (TApplied f t) (TApplied f' t') = do
  sub <- matches f f'
  s <- apply sub t `matches` apply sub t'
  return $ s `compose` sub

matches t (TVar a) = bind a t
matches (TVar a) t = bind a t

matches t t' | kind t /= kind t' = throwError $ Fail "Kind mismatch"
matches t1 t2 = case (t1, t2) of
  (t@(TClosure {}), t') -> refine t `matches` t'
  (t, t'@(TClosure {})) -> t `matches` refine t'
  _                     -> throwError $ UnificationFail t1 t2
  where
    refine (TClosure params tyExpr env) = fromRight err $ Refine.runIn (env `Map.union` env') tyExpr
      where
        tvars = fmap (`Tyvar` KType) params
        env'  = Map.fromList $ zip params (fmap TVar tvars)
        err   = error "Failed to refine TClosure while matching"


bind :: (MonadError e m, e ~ InferenceError) => Tyvar -> Type -> m Subst
-- bind a t | trace ("Binding: " ++ show a ++ " to " ++ show t) False = undefined
bind a t
  | t == TVar a = return nullSubst
  | occursCheck a t = throwError $ InfiniteType a t
  | kind a /= kind t = throwError $ Fail "kinds do not match"
  | TLiteral l <- t = return . Map.singleton a $
      case l of
        LInt _    -> TPrimitive TInt
        LString _ -> TPrimitive TString
        LBool _   -> TPrimitive TBool
  | otherwise = return $ Map.singleton a t

occursCheck :: Substitutable a => Tyvar -> a -> Bool
occursCheck a t = a `Set.member` set
  where
    set = ftv t
