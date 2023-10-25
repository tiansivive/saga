module Saga.Language.Typechecker.Solver.Substitution where

import qualified Data.Map                            as Map
import qualified Data.Set                            as Set

import           Saga.Language.Typechecker.Variables (PolymorphicVar)



type Subst t = Map.Map (PolymorphicVar t) t

-- | Substitute t in data structure a
class Substitutable a t where
    apply :: Subst t -> a -> a
    ftv :: a -> Set.Set (PolymorphicVar t)

instance {-# OVERLAPPABLE #-} (Functor f, Foldable f, Substitutable a t, Ord t) => Substitutable (f a) t where
    apply = fmap . apply
    ftv = foldl union Set.empty
        where union set x = Set.union (ftv x) set


compose :: (Ord t, Substitutable t t) => Subst t -> Subst t -> Subst t
s1 `compose` s2 = s `Map.union` s1
  where
    s = Map.map (apply s1) s2

nullSubst :: forall t. Subst t
nullSubst = Map.empty
