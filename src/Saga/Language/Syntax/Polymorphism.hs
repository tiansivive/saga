{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Saga.Language.Syntax.Polymorphism where

import Data.Map (Map)
import qualified Data.Map as Map

import Saga.Language.Typechecker.Variables (Variable)



data Polymorphic t = Forall [Variable t] t
deriving instance (Show t, Show (Variable t)) => Show (Polymorphic t)
deriving instance (Eq t, Eq (Variable t))     => Eq (Polymorphic t)
deriving instance (Ord t, Ord (Variable t))   => Ord (Polymorphic t)

data Qualified t = (:=>)
    { given :: Given t
    , item:: t
    } 
deriving instance (Show t, Show (Given t)) => Show (Qualified t)
deriving instance (Eq t, Eq (Given t)) => Eq (Qualified t)
deriving instance (Ord t, Ord (Given t)) => Ord (Qualified t)
infixl 1 :=>


data Given t = (:|)
  { bindings:: Map (Variable t) t
  , constraints :: [Qualifier t]
  }
deriving instance (Show t, Show (Variable t), Show (Qualifier t)) => Show (Given t)
deriving instance (Eq t, Eq (Variable t), Eq (Qualifier t)) => Eq (Given t)
deriving instance (Ord t, Ord (Variable t), Ord (Qualifier t)) => Ord (Given t)
infixl 1 :|

type family Qualifier t 


none :: Given t
none = Map.empty :| []
