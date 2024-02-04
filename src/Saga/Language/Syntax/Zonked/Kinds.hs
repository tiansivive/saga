{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE TypeFamilies       #-}

module Saga.Language.Syntax.Zonked.Kinds where


import qualified Saga.Language.Syntax.AST            as NT (NodeType (..))
import           Saga.Language.Syntax.AST            hiding (NodeType (..))
import           Saga.Language.Syntax.Liquids
import           Saga.Language.Syntax.Zonked.AST

import           Data.Map                            (Map)
import           Saga.Language.Syntax.Literals

import           Data.Data
import           Saga.Language.Typechecker.Variables (Variable)
import           Saga.Utils.TypeLevel                (type (§))

type Kind = Node Zonked NT.Kind
data instance Node Zonked NT.Kind where
    Type        :: Kind
    Kind        :: Kind
    Constraint  :: Kind
    Protocol    :: Kind -> Kind
    Var         :: Variable Kind  -> Kind
    Arrow       :: Kind -> Kind   -> Kind
    Application :: Kind -> [Kind] -> Kind
deriving instance Show Kind
deriving instance Show (AST Zonked NT.Kind)
deriving instance Eq Kind
deriving instance Eq (AST Zonked NT.Kind)
deriving instance Ord Kind
deriving instance Ord (AST Zonked NT.Kind)
deriving instance Data Kind
deriving instance Data (AST Zonked NT.Kind)

data instance Variable Kind where
    Poly        :: String -> Variable Kind
deriving instance Show (Variable Kind)
deriving instance Eq (Variable Kind)
deriving instance Ord (Variable Kind)
deriving instance Data (Variable Kind)
