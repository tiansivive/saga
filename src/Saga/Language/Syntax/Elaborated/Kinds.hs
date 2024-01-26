{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Syntax.Elaborated.Kinds where


import qualified Saga.Language.Syntax.AST            as NT (NodeType (..))
import           Saga.Language.Syntax.AST
import           Saga.Language.Syntax.Elaborated.AST
import           Saga.Language.Syntax.Liquids

import           Data.Map                            (Map)
import           Saga.Language.Syntax.Literals

import           Saga.Language.Syntax.Polymorphism   (Qualifier)
import           Saga.Language.Typechecker.Variables (Variable)
import           Saga.Utils.TypeLevel                (type (§))


type Kind = Node Elaborated NT.Kind
data instance Node Elaborated NT.Kind where
    Type        :: Kind
    Kind        :: Kind
    Constraint  :: Kind
    Protocol    :: Kind -> Kind
    Var         :: Variable Kind  -> Kind
    Arrow       :: Kind -> Kind   -> Kind
    Application :: Kind -> [Kind] -> Kind
deriving instance Show Kind
deriving instance Eq Kind
deriving instance Ord Kind

deriving instance Show (AST Elaborated NT.Kind)
deriving instance Eq (AST Elaborated NT.Kind)
deriving instance Ord (AST Elaborated NT.Kind)

data instance Variable Kind where
    Poly        :: String -> Variable Kind
    Unification :: String -> Variable Kind
deriving instance Show (Variable Kind)
deriving instance Eq (Variable Kind)
deriving instance Ord (Variable Kind)

type instance Qualifier Kind = ()

