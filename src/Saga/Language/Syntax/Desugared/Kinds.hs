
{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Syntax.Desugared.Kinds where


import qualified Saga.Language.Syntax.AST            as NT (NodeType (..))
import           Saga.Language.Syntax.AST

import           Saga.Language.Syntax.Desugared.AST
import           Saga.Language.Syntax.Liquids
import           Saga.Language.Syntax.Literals

import           Saga.Language.Typechecker.Variables (Variable)

import           Data.Map                            (Map)
import           Saga.Utils.TypeLevel                (type (§))


type KindExpr = Node Desugared NT.Kind

data instance Node Desugared NT.Kind where
    Identifier  :: String                   -> KindExpr
    Arrow       :: KindExpr ->  KindExpr    -> KindExpr
    Application :: KindExpr -> [KindExpr]   -> KindExpr
deriving instance Show KindExpr
deriving instance Show (AST Desugared NT.Kind)
deriving instance Eq KindExpr
deriving instance Eq (AST Desugared NT.Kind)
deriving instance Ord KindExpr
deriving instance Ord (AST Desugared NT.Kind)
