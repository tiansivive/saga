{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Syntax.Elaborated.Types where

import           Data.Map                              (Map)
import qualified Saga.Language.Syntax.AST              as NT (NodeType (..))
import           Saga.Language.Syntax.AST
import           Saga.Language.Syntax.Elaborated.AST
import           Saga.Language.Syntax.Polymorphism

import           Saga.Language.Syntax.Literals

import           Saga.Language.Syntax.Elaborated.Kinds
import           Saga.Language.Syntax.Liquids
import           Saga.Language.Typechecker.Variables   (Variable)

import           Saga.Utils.TypeLevel                  (type (§))

type Type = Node Elaborated NT.Type
data instance Node Elaborated NT.Type where
    Singleton   :: Literal                                              -> Type
    Tuple       :: [AST Elaborated NT.Type]                             -> Type
    Record      :: [(String, AST Elaborated NT.Type)]                   -> Type
    Union       :: [AST Elaborated NT.Type]                             -> Type
    Arrow       :: Type -> Type                                         -> Type
    Data        :: String -> Kind                                       -> Type
    Applied     :: AST Elaborated NT.Type -> AST Elaborated NT.Type     -> Type
    Var         :: Variable Type                                        -> Type
    Polymorphic :: Polymorphic Type                                     -> Type
    --Closure     :: [Variable Type] -> TypeExpr -> Scope   -> Type
    Void        :: Type
    Any         :: Type
deriving instance Show Type
deriving instance Eq Type
deriving instance Ord Type

deriving instance Show (AST Elaborated NT.Type)
deriving instance Eq (AST Elaborated NT.Type)
deriving instance Ord (AST Elaborated NT.Type)


data instance Variable Type where
  Poly              :: String -> Kind -> Variable Type
  Existential       :: String -> Kind -> Variable Type
  Local             :: String -> Kind -> Variable Type
  Skolem            :: String -> Kind -> Variable Type
  Rigid             :: String -> Kind -> Variable Type
  Scoped            :: String -> Kind -> Variable Type
  Unification       :: String -> Kind -> Variable Type
  Instantiation     :: String -> Kind -> Variable Type
deriving instance Show (Variable Type)
deriving instance Eq (Variable Type)
deriving instance Ord (Variable Type)


type TypeConstraint = Node Elaborated NT.Constraint
data instance Node Elaborated NT.Constraint where
  Implements :: Type -> ProtocolID -> Node Elaborated NT.Constraint
  Refinement :: Bindings -> Liquid  -> Type -> Node Elaborated NT.Constraint


type Bindings = Map (Variable Liquid) Type
type ProtocolID = String

deriving instance Show TypeConstraint
deriving instance Eq TypeConstraint
deriving instance Ord TypeConstraint

deriving instance Show (AST Elaborated NT.Constraint)

type instance Qualifier Type = TypeConstraint

