{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}



module Saga.Language.Typechecker.Variables where
import qualified Data.Kind as GHC


type Classifiable t = (Show (Classifier t), Eq (Classifier t), Ord (Classifier t))



data PolymorphicVar t where

  Type             :: Classifiable t => String -> Classifier t -> PolymorphicVar t
  Kind             :: Classifiable t => String -> Classifier t -> PolymorphicVar t

  Skolem           :: Classifiable t => String -> Classifier t -> PolymorphicVar t
  Unification      :: Classifiable t => String -> Level -> Classifier t -> PolymorphicVar t
  Instantiation    :: Classifiable t => String -> PolymorphicVar t
  Evidence         :: Restricted t   => String -> PolymorphicVar t
  Refinement       :: Restricted t   => String -> PolymorphicVar t
  Local            :: Classifiable t => String -> Classifier t -> PolymorphicVar t

deriving instance Eq t    => Eq   (PolymorphicVar t)
deriving instance Ord t   => Ord  (PolymorphicVar t)
deriving instance Show t  => Show (PolymorphicVar t)



newtype Level = Level Int deriving (Show, Eq, Ord)

type family Classifier a  :: GHC.Type
type family VarType e a   :: GHC.Type
-- | ENHANCEMENT We want different constraints for different constructors.
-- | TODO #25
-- | Right now, if both refinements and evidence implement an instance of `Restricted`, then a refinement var could be used where an evidence var is expected, and vice-versa
-- | SUGGESTION More type families - one for each.
-- | SUGGESTION One more elaborate type family: `Context`
-- | QUESTION: How to implement a `Context` type family?
type family Restricted a  :: GHC.Constraint
