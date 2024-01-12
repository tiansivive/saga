{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Syntax.Zonked.Kinds where


import qualified Saga.Language.Syntax.AST            as NT (NodeType (..))
import           Saga.Language.Syntax.AST
import           Saga.Language.Syntax.Desugared.AST
import           Saga.Language.Syntax.Liquids

import           Data.Map                            (Map)
import           Saga.Language.Syntax.Literals

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

data instance Variable Kind where
    Poly        :: String -> Kind -> Variable Kind
deriving instance Show (Variable Kind)
