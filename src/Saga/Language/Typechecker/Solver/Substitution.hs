{-# LANGUAGE TypeFamilies #-}
module Saga.Language.Typechecker.Solver.Substitution where

import qualified Data.Map                            as Map
import qualified Data.Set                            as Set

import           Saga.Language.Typechecker.Variables (Variable)



type Subst t = Map.Map (Variable t) t

-- | Substitute t in data structure a
class Substitutable a where
    type Target a
    apply :: (t ~ Target a) => Subst t -> a -> a
    ftv   :: (t ~ Target a, Ord (Variable t)) =>  a -> Set.Set (Variable t)

instance {-# OVERLAPPABLE #-} (Functor f, Foldable f, Substitutable a) => Substitutable (f a) where
    type Target (f a) = Target a
    apply = fmap . apply
    ftv = foldl union Set.empty
        where union set x = Set.union (ftv x) set



compose :: (Target t ~ t, Substitutable t, Ord (Variable t)) => Subst t -> Subst t -> Subst t
s1 `compose` s2 = s `Map.union` s1
  where
    s = Map.map (apply s1) s2

nullSubst :: forall t. Subst t
nullSubst = Map.empty

mkSubst :: Ord (Variable t) => (Variable t, t) -> Subst t
mkSubst (v, t) = Map.insert v t Map.empty
