
{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Syntax.Reduced.Kinds where


import qualified Saga.Language.Syntax.AST            as NT (NodeType (..))
import           Saga.Language.Syntax.AST

import           Saga.Language.Syntax.Liquids
import           Saga.Language.Syntax.Literals
import           Saga.Language.Syntax.Reduced.AST

import           Saga.Language.Typechecker.Variables (Variable)

import           Data.Data                           (Data)
import           Data.Map                            (Map)
import           Saga.Utils.TypeLevel                (type (§))


type KindExpr = Node Reduced NT.Kind

data instance Node Reduced NT.Kind where
    Identifier  :: String                   -> KindExpr
    Arrow       :: KindExpr ->  KindExpr    -> KindExpr
    Application :: KindExpr -> [KindExpr]   -> KindExpr
deriving instance Show KindExpr
deriving instance Show (AST Reduced NT.Kind)
deriving instance Eq KindExpr
deriving instance Eq (AST Reduced NT.Kind)
deriving instance Ord KindExpr
deriving instance Ord (AST Reduced NT.Kind)
deriving instance Data KindExpr
deriving instance Data (AST Reduced NT.Kind)
