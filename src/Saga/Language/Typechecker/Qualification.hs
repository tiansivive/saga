module Saga.Language.Typechecker.Qualification where


import qualified Saga.Language.Core.Liquid as Liquid


data Qualified t = (:=>)
    { constraints :: [Constraint t]
    , item:: t
    } deriving (Show, Eq, Ord)
infixr 0 :=>


data Constraint a
  = a `Implements` String
  | Resource Multiplicity a
  | Refinement Liquid.Expr a
  | Pure a
  | Equality a a
  deriving (Show, Eq, Ord)


data Multiplicity
  = Erased
  | Unrestricted
  | Linear
  | Affine
  | Precise Int
  | Bounded Int
  deriving (Eq, Show, Ord)


instance Functor Qualified where
  fmap f (cs :=> q) = (fmap . fmap) f cs :=> f q

instance Functor Constraint where
  fmap f (Pure a)          = Pure $ f a
  fmap f (Resource m a)    = Resource m $ f a
  fmap f (Refinement re a) = Refinement re $ f a
  fmap f (Implements a p)  = Implements (f a) p

instance Applicative Qualified where
  pure = (:=>) []
  (cs :=> f) <*> (cs' :=> a) = zipWith apply cs cs' :=> f a


-- instance Monad Qualified where
--   (cs :=> q) >>= f = zipWith apply cs cs' :=> q'
--     where
--       (cs' :=> q') = f q
-- -- instance Monad Const where
-- --   (>>=) (Pure a) f = f a

apply (Pure f) (Pure a)                                 = Pure $ f a
apply (Resource m f) (Resource m' a)        | m == m'   = Resource m $ f a
apply (Refinement re f) (Refinement re' a)  | re == re' = Refinement re $ f a
apply (Implements f p) (Implements a p' )   | p == p'   = Implements (f a) p
apply _ _ = error "No applicative instance for Qualified t when applying different Constraints"
