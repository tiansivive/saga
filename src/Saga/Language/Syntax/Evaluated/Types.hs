{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Syntax.Evaluated.Types where

import           Data.Map                             (Map)

import qualified Saga.Language.Syntax.AST             as NT (NodeType (..))
import           Saga.Language.Syntax.AST

import           Saga.Language.Syntax.Liquids
import           Saga.Language.Syntax.Literals

import           Saga.Language.Syntax.Desugared.Types (TypeExpr)
import           Saga.Language.Syntax.Evaluated.AST
import           Saga.Language.Syntax.Evaluated.Kinds
import           Saga.Language.Syntax.Polymorphism
import           Saga.Language.Typechecker.Variables  (Variable)

import           Saga.Utils.TypeLevel                 (type (§))


type Type = Node Evaluated NT.Type
data instance Node Evaluated NT.Type where
    Singleton   :: Literal                                -> Type
    Tuple       :: [Type]                                 -> Type
    Record      :: [(String, Type)]                       -> Type
    Union       :: [Type]                                 -> Type
    Arrow       :: Type -> Type                           -> Type
    Data        :: String -> Kind                         -> Type
    Applied     :: Type -> Type                           -> Type
    Var         :: Variable Type                          -> Type
    Polymorphic :: Polymorphic Type                       -> Type
    Closure     :: [Variable Type] -> TypeExpr -> Scope   -> Type
    Void        :: Type
    Any         :: Type
deriving instance Show Type
deriving instance Show (AST Evaluated NT.Type)

data Scope = Scope
  { types :: Map String (Polymorphic Type)
  , kinds :: Map String (Polymorphic Kind)
  } deriving (Show)


data instance Variable Type where
  Poly              :: String -> Kind -> Variable Type
  Existential       :: String -> Kind -> Variable Type
  Local             :: String -> Kind -> Variable Type
deriving instance Show (Variable Type)

type TypeConstraint = Node Evaluated NT.Constraint
data instance Node Evaluated NT.Constraint where
deriving instance Show TypeConstraint

type instance Qualifier Type = TypeConstraint

