{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Saga.Language.Syntax.Polymorphism where

import Data.Map (Map)
import qualified Data.Map as Map

import Saga.Language.Typechecker.Variables (Variable)



data Polymorphic t = Forall [Variable t] (Qualified t)
deriving instance (Show t, Show (Variable t), Show (Qualified t)) => Show (Polymorphic t)

data Qualified t = (:=>)
    { given :: Given t
    , item:: t
    } 
deriving instance (Show t, Show (Given t)) => Show (Qualified t)
infixl 1 :=>


data Given t = (:|)
  { bindings:: Map (Variable t) (Qualified t)
  , constraints :: [Qualifier t]
  }
deriving instance (Show t, Show (Variable t), Show (Qualifier t)) => Show (Given t)
infixl 1 :|


type family Qualifier t 



none :: Given t
none = Map.empty :| []
