{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Syntax.Evaluated.Kinds where

import qualified Saga.Language.Syntax.AST            as NT (NodeType (..))
import           Saga.Language.Syntax.AST
import           Saga.Language.Syntax.Evaluated.AST


import           Saga.Language.Syntax.Polymorphism
import           Saga.Language.Typechecker.Variables
import           Saga.Utils.TypeLevel                (type (§))


type Kind = Node Evaluated NT.Kind
data instance Node Evaluated NT.Kind where
    Type        :: Kind
    Kind        :: Kind
    Constraint  :: Kind
    Protocol    :: Kind -> Kind
    Arrow       :: Kind -> Kind     -> Kind
    Application :: Kind -> [Kind]   -> Kind
    Var         :: Variable Kind    -> Kind
    Polymorphic :: Polymorphic Kind -> Kind
deriving instance Show Kind
deriving instance Eq Kind
deriving instance Ord Kind

deriving instance Show (AST Evaluated NT.Kind)
deriving instance Eq (AST Evaluated NT.Kind)
deriving instance Ord (AST Evaluated NT.Kind)

data instance Variable Kind where
    Poly        :: String -> Kind -> Variable Kind
deriving instance Show (Variable Kind)
deriving instance Eq (Variable Kind)
deriving instance Ord (Variable Kind)


type instance Qualifier Kind = ()
