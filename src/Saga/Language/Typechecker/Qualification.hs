module Saga.Language.Typechecker.Qualification where
import qualified Saga.Language.Typechecker.Refinement.Liquid as Liquid
import Saga.Language.Typechecker.Variables (PolymorphicVar)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Bifunctor (Bifunctor(bimap))




data Qualified t = (:=>)
    { given :: Given t
    , item:: t
    } deriving (Show, Eq, Ord)
infixl 1 :=>

data Given t = (:|)
  { bindings:: Map (PolymorphicVar t) (Qualified t)
  , constraints :: [Constraint t]
  } deriving (Show, Eq, Ord)
infixl 1 :|

data Constraint a
  = a `Implements` String
  | Resource Multiplicity a
  | Refinement (Binding a) Liquid.Liquid a
  | Pure a
  | Equality a a
  deriving (Show, Eq, Ord)

type Binding a = Map String a

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


