{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Saga.Language.TypeSystem.Kinds where
import           Control.Applicative                  ((<|>))
import           Control.Monad.Except
import           Control.Monad.RWS
import qualified Data.Map                             as Map
import qualified Data.Set                             as Set
import           Debug.Trace
import           Prelude                              hiding (EQ)

import           Saga.Language.TypeSystem.Errors      (SagaError (..))


import           Saga.Language.TypeSystem.Environment

import qualified Saga.Language.TypeSystem.Refinement  as Refine
import           Saga.Language.TypeSystem.Shared

import           Saga.Language.TypeSystem.Types
import           Saga.Utils.Utils

type KSubst = Map.Map String Kind
-- type Infer = RWST CompilerState Trace InferState (Except SagaError)

data Trace = Traced Accumulator [KindUnification] deriving (Show)

instance Semigroup Trace where
  (Traced acc1 cs1) <> (Traced acc2 cs2) = Traced (acc1 <> acc2) (cs1 <> cs2)
instance Monoid Trace where
  mempty = Traced mempty []


run :: Kind -> Kind -> Either SagaError KSubst
run k1 k2 = runExcept $ unify k1 k2


-- unification :: KSubst -> [Equality Kind] -> Solve KSubst
-- unification s cs | trace ("\nKind Unification:" ++ "\n\tUnifier:\n\t" ++ pretty s ++ "\n\t" ++ printEqs cs) False = undefined
-- unification s [] = return s
-- unification s (e:es) | t1 `EQ` t2 <- e = do
--   sub <- unify t1 t2
--   let sub' = compose sub s
--   sub'' <- unification sub' $ fmap (apply' sub') es
--   return $ sub'' `compose` sub'
--     where
--       apply' s (k1 `EQ` k2) = apply s k1 `EQ` apply s k2


unify :: (Monad m, MonadError SagaError m) => Kind -> Kind -> m KSubst
unify KType KType = return Map.empty
unify (KArrow inK1 outK1) (KArrow inK2 outK2) = do
  sub <- unify inK1 inK2
  s <- unify (apply sub outK1) (apply sub outK2)
  return $ s `compose` sub

unify (KProtocol k1) (KProtocol k2) = unify k1 k2
unify k (KVar v) = bind v k
unify (KVar v) k = bind v k

unify k1 k2 = throwError $ UnificationKindFail k1 k2


bind :: (Monad m, MonadError SagaError m) => String -> Kind -> m KSubst
bind id k
    | KVar id == k  = return Map.empty
    | occurs id k   = throwError $ InfiniteKind id k
    | otherwise     = return $ Map.singleton id k

occurs id k = id `Set.member` ftv k





apply :: KSubst -> Kind -> Kind
apply sub k@(KVar id)       = Map.findWithDefault k id sub
apply sub (KArrow inK outK) = KArrow (apply sub inK) (apply sub outK)
apply sub (KProtocol k)     = KProtocol (apply sub k)
apply sub k                 = k

ftv (KVar id)      = Set.singleton id
ftv (KArrow k1 k2) = ftv k1 `Set.union` ftv k2
ftv (KProtocol k)  = ftv k
ftv k              = Set.empty

compose :: KSubst -> KSubst -> KSubst
s1 `compose` s2 = s `Map.union` s1
  where
    s = Map.map (apply s1) s2



infer :: InferM Trace m => TypeExpr -> m Kind
infer (TAtom ty) = do
  env <- ask
  return $ kind env ty

infer (TTagged _ ty) = infer ty
infer (TClause ty binds) = infer ty
infer (TImplementation pid ty) = infer ty
infer (TComposite composite) = return KType
infer (TLambda params body) = do
  kBody <- infer body
  return $ foldr (KArrow . KVar) kBody params
infer (TQualified (cs :=> ty)) = infer ty
infer (TIdentifier ty) = lookupK ty
infer (TFnApp fn [arg]) = do
  out <- freshKind
  fn' <- infer fn
  arg' <- infer arg
  let inferred = arg' `KArrow` out
  emit $ fn' `UnifyK` inferred

  return out
infer (TFnApp fn (a : as)) = infer curried
  where
    partial = TFnApp fn [a]
    curried = foldl (\f a -> TFnApp f [a]) partial as

infer tyExpr = throwError $ Fail $ "Kind inference not implemented yet for type expressions like: " ++ show tyExpr


class HasKind t where
  kind :: CompilerState -> t -> Kind

instance HasKind Tyvar where
  kind _ (Tyvar _ k) = k

instance HasKind Tycon where
  kind _ (Tycon _ k) = k

instance HasKind Type where
  kind env (TData cons) = kind env cons
  kind env (TVar u)     = kind env u
  kind env (TApplied f _) = case k' of
      (KArrow _ k) -> k
    where k' = kind env f
  kind env (TClosure ps tyExpr closure) = case refined of
    Left err -> error err
    Right ty -> foldr (KArrow . KVar) (kind env ty) ps
    where
      refined = Refine.runIn env tyExpr
  kind env _ = KType
  --   (KArrow _ k) -> k



lookupK :: InferM Trace m => String -> m Kind
lookupK x | trace ("\nLooking up: " ++ x) False = undefined
lookupK x = do
  Saga { kinds } <- ask
  IST { kUnification } <- get
  case Map.lookup x kinds <|> Map.lookup x kUnification of
    Just k  -> return k
    Nothing -> throwError $ UnboundVariable (show x)


freshKind ::  InferM Trace m => m Kind
freshKind = do
  modify $ \s -> s {kvars = kvars s + 1}
  s <- get
  let v = "k" ++ show ([1 ..] !! kvars s)
  return $ KVar v

emit :: InferM Trace m => KindUnification -> m ()
emit c = tell $ Traced mempty (pure c)
