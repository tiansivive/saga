{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeFamilies     #-}




module Saga.Language.Typechecker.Variables where
import qualified Data.Kind as GHC


type Classifiable t = (Show (Classifier t), Eq (Classifier t), Ord (Classifier t))
data family Variable t


type family Classifier a  :: GHC.Type
type family VarType e a   :: GHC.Type

newtype Level = Level Int deriving (Show, Eq, Ord, Num)

