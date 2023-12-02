{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeFamilies     #-}




module Saga.Language.Typechecker.Variables where
import qualified Data.Kind as GHC


type Classifiable t = (Show (Classifier t), Eq (Classifier t), Ord (Classifier t))
data family PolymorphicVar t

newtype Level = Level Int deriving (Show, Eq, Ord)

type family Classifier a  :: GHC.Type
type family VarType e a   :: GHC.Type
-- | ENHANCEMENT We want different constraints for different constructors.
-- | TODO #25
-- | Right now, if both refinements and evidence implement an instance of `Restricted`, then a refinement var could be used where an evidence var is expected, and vice-versa
-- | SOLUTION Data family
-- | QUESTION How to properly separate instances without generating duplicate names or cyclic dependencies


