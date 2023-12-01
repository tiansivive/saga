{-# LANGUAGE TypeFamilies #-}
module Saga.Language.Typechecker.Solver.Substitution where

import qualified Data.Map                            as Map
import qualified Data.Set                            as Set

import           Saga.Language.Typechecker.Variables (PolymorphicVar)



type Subst t = Map.Map (PolymorphicVar t) t

-- | Substitute t in data structure a
class Substitutable a where
    type Target a
    apply :: (t ~ Target a) => Subst t -> a -> a
    ftv   :: (t ~ Target a, Ord t) =>  a -> Set.Set (PolymorphicVar t)

instance {-# OVERLAPPABLE #-} (Functor f, Foldable f, Substitutable a) => Substitutable (f a) where
    type Target (f a) = Target a
    apply = fmap . apply
    ftv = foldl union Set.empty
        where union set x = Set.union (ftv x) set



compose :: (Target t ~ t, Substitutable t, Ord t) => Subst t -> Subst t -> Subst t
s1 `compose` s2 = s `Map.union` s1
  where
    s = Map.map (apply s1) s2

nullSubst :: forall t. Subst t
nullSubst = Map.empty

mkSubst :: Ord t => (PolymorphicVar t, t) -> Subst t
mkSubst (v, t) = Map.insert v t Map.empty
