{-# LANGUAGE UndecidableInstances #-}
module Saga.Language.Typechecker.Qualification where
import Saga.Language.Typechecker.Refinement.Liquid (Liquid)
import Saga.Language.Typechecker.Variables (Variable)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Bifunctor (Bifunctor(bimap))




data Qualified t = (:=>)
    { given :: Given t
    , item:: t
    } 
infixl 1 :=>
deriving instance (Show t, Show (Given t)) => Show (Qualified t)
deriving instance (Eq t, Eq (Given t)) => Eq (Qualified t)
deriving instance (Ord t, Ord (Given t)) => Ord (Qualified t)

data Given t = (:|)
  { bindings:: Map (Variable t) (Qualified t)
  , constraints :: [Constraint t]
  }
infixl 1 :|
deriving instance (Show t, Show (Variable t)) => Show (Given t) 
deriving instance (Eq t, Eq (Variable t)) => Eq (Given t)
deriving instance (Ord t, Ord (Variable t)) => Ord (Given t)

data Constraint a
  = a `Implements` String
  | Resource Multiplicity a
  | Refinement (Binding a) Liquid a
  | Pure a
  | Equality a a
  deriving (Show, Eq, Ord)

type Binding a = Map (Variable Liquid) a

data Multiplicity
  = Erased
  | Unrestricted
  | Linear
  | Affine
  | Precise Int
  | Bounded Int
  deriving (Eq, Show, Ord)


instance Functor Constraint where
  fmap f (Pure a)          = Pure $ f a
  fmap f (Resource m a)    = Resource m $ f a
  fmap f (Refinement bs l a) = Refinement (fmap f bs) l $ f a
  fmap f (Implements a p)  = Implements (f a) p


none :: Given t
none = Map.empty :| []

constrained :: [Constraint t] -> Given t
constrained = (:|) Map.empty


