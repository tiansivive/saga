{-# LANGUAGE FlexibleContexts #-}

module Saga.Language.TypeSystem.Kinds where
import           Control.Monad.Except
import qualified Data.Map                        as Map
import qualified Data.Set                        as Set
import           Saga.Language.TypeSystem.Errors (SagaError (..))
import           Saga.Language.TypeSystem.Types  (Kind (..))

type KSubst = Map.Map String Kind


run :: Kind -> Kind -> Either SagaError KSubst
run k1 k2 = runExcept $ unify k1 k2

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



