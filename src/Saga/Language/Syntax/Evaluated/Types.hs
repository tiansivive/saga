{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Syntax.Evaluated.Types where

import           Data.Map                             (Map)

import qualified Saga.Language.Syntax.AST             as NT (NodeType (..))
import           Saga.Language.Syntax.AST             hiding (NodeType (..))

import           Saga.Language.Syntax.Liquids
import           Saga.Language.Syntax.Literals

import           Saga.Language.Syntax.Desugared.Types (TypeExpr)
import           Saga.Language.Syntax.Evaluated.AST
import           Saga.Language.Syntax.Evaluated.Kinds hiding (Kind)
import           Saga.Language.Syntax.Polymorphism
import           Saga.Language.Typechecker.Variables  (Variable)

import           Saga.Language.Syntax.Evaluated.Kinds (Kind)
import           Saga.Utils.TypeLevel                 (type (§))


type Type = Node Evaluated NT.Type
data instance Node Evaluated NT.Type where
    Singleton   :: Literal                                            -> Type
    Tuple       :: [AST Evaluated NT.Type]                            -> Type
    Record      :: [(String, AST Evaluated NT.Type)]                  -> Type
    Union       :: [AST Evaluated NT.Type]                            -> Type
    Data        :: String -> Kind                                     -> Type
    Applied     :: AST Evaluated NT.Type -> AST Evaluated NT.Type     -> Type
    Arrow       :: Type -> Type                                       -> Type
    Var         :: Variable Type                                      -> Type
    Polymorphic :: Polymorphic Type                                   -> Type
    Qualified   :: Qualified Type                                     -> Type
    Closure     :: [Variable Type] -> TypeExpr -> Scope               -> Type
    Void        :: Type
    Any         :: Type
deriving instance Show Type
deriving instance Show (AST Evaluated NT.Type)

data Scope = Scope
  { types :: Map String (AST Evaluated NT.Type)
  , kinds :: Map String (AST Evaluated NT.Kind)
  } deriving (Show)


data instance Variable Type where
  Poly              :: String -> Kind -> Variable Type
  Existential       :: String -> Kind -> Variable Type
  Local             :: String -> Kind -> Variable Type
deriving instance Show (Variable Type)
deriving instance Eq (Variable Type)
deriving instance Ord (Variable Type)

type TypeConstraint = Node Evaluated NT.Constraint
data instance Node Evaluated NT.Constraint where
  Implements :: Type -> ProtocolID -> Node Evaluated NT.Constraint
  Refinement :: Bindings -> Liquid  -> Type -> Node Evaluated NT.Constraint


type Bindings = Map (Variable Liquid) TypeExpr
type ProtocolID = String

deriving instance Show TypeConstraint
deriving instance Show (AST Evaluated NT.Constraint)

type instance Qualifier Type = TypeConstraint

