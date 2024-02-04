{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Saga.Language.Syntax.Zonked.Types where

import           Data.Map                            (Map)
import qualified Saga.Language.Syntax.AST            as NT (NodeType (..))
import           Saga.Language.Syntax.AST            hiding (NodeType (..))

import           Saga.Language.Syntax.Polymorphism
import           Saga.Language.Syntax.Zonked.AST

import           Saga.Language.Syntax.Literals

import           Saga.Language.Syntax.Zonked.Kinds
import           Saga.Language.Typechecker.Variables (Variable)

import           Saga.Utils.TypeLevel                (type (§))

import Data.Data

type Type = Node Zonked NT.Type
data instance Node Zonked NT.Type where
    Singleton   :: Literal                                      -> Type
    Tuple       :: [AST Zonked NT.Type]                         -> Type
    Record      :: [(String, AST Zonked NT.Type)]               -> Type
    Union       :: [AST Zonked NT.Type]                         -> Type
    Arrow       :: AST Zonked NT.Type -> AST Zonked NT.Type     -> Type
    Data        :: String                                -> Type
    Applied     :: AST Zonked NT.Type -> AST Zonked NT.Type     -> Type
    Var         :: Variable Type                                -> Type
    Polymorphic :: Polymorphic Type                             -> Type
    --Closure     :: [Variable Type] -> TypeExpr -> Scope   -> Type
    Void        :: Type
    Any         :: Type
deriving instance Show Type
deriving instance Show (AST Zonked NT.Type)
deriving instance Data Type
deriving instance Data (AST Zonked NT.Type)

data instance Variable Type where
  Poly              :: String -> Kind -> Variable Type
  Existential       :: String -> Kind -> Variable Type
  Local             :: String -> Kind -> Variable Type
deriving instance Show (Variable Type)
deriving instance Eq (Variable Type)
deriving instance Ord (Variable Type)
deriving instance Data (Variable Type)



type TypeConstraint = Node Zonked NT.Constraint
data instance Node Zonked NT.Constraint where
deriving instance Show TypeConstraint

type instance Qualifier Type = TypeConstraint



