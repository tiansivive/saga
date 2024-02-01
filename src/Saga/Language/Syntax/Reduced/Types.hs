{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Syntax.Reduced.Types where

import           Data.Map                            (Map)

import qualified Saga.Language.Syntax.AST            as NT (NodeType (..))
import           Saga.Language.Syntax.AST            hiding (NodeType (..))

import           Saga.Language.Syntax.Liquids
import           Saga.Language.Syntax.Literals

import           Saga.Language.Syntax.Polymorphism
import           Saga.Language.Syntax.Reduced.AST
import           Saga.Language.Syntax.Reduced.Kinds

import           Saga.Language.Typechecker.Variables (Variable)

import           Saga.Utils.TypeLevel                (type (§))


type TypeExpr = Node Reduced NT.Type
data instance Node Reduced NT.Type where
    Singleton     :: Literal -> TypeExpr
    Identifier    :: String  -> TypeExpr

    Union         :: [TypeExpr] -> TypeExpr
    Tuple         :: [TypeExpr] -> TypeExpr
    Record        :: [(String, TypeExpr)] -> TypeExpr
    Arrow         :: TypeExpr -> TypeExpr -> TypeExpr

    Match            :: TypeExpr -> [Node Reduced (NT.Case NT.Type)] -> TypeExpr
    Clause           :: TypeExpr -> [Binding] -> TypeExpr
    Tagged           :: String -> TypeExpr -> TypeExpr

    Lambda           :: [String] -> TypeExpr -> TypeExpr
    Application      :: TypeExpr -> [TypeExpr] -> TypeExpr

    Implementation   :: ProtocolID -> TypeExpr -> TypeExpr
deriving instance Show TypeExpr
deriving instance Show (AST Reduced NT.Type)
deriving instance Eq TypeExpr
deriving instance Eq (AST Reduced NT.Type)
deriving instance Ord TypeExpr
deriving instance Ord (AST Reduced NT.Type)


data instance Node Reduced (NT.Case NT.Type) where
  Case :: AST Reduced (NT.Pattern NT.Type) -> AST Reduced (NT.Case NT.Type) -> Node Reduced (NT.Case NT.Type)
deriving instance Show (Node Reduced (NT.Case NT.Type))
deriving instance Show (AST Reduced (NT.Case NT.Type))
deriving instance Eq (Node Reduced (NT.Case NT.Type))
deriving instance Eq (AST Reduced (NT.Case NT.Type))
deriving instance Ord (Node Reduced (NT.Case NT.Type))
deriving instance Ord (AST Reduced (NT.Case NT.Type))

data instance Node Reduced (NT.Pattern NT.Type) where
  Wildcard  :: Node Reduced (NT.Pattern NT.Type)
  Id        :: String                                                               -> Node Reduced (NT.Pattern NT.Type)
  PatHole   :: String                                                               -> Node Reduced (NT.Pattern NT.Type)
  PatLit    :: Literal                                                              -> Node Reduced (NT.Pattern NT.Type)
  PatArrow  :: [AST Reduced (NT.Pattern NT.Type)]                                    -> Node Reduced (NT.Pattern NT.Type)
  PatTuple  :: [AST Reduced (NT.Pattern NT.Type)]                   -> Maybe String  -> Node Reduced (NT.Pattern NT.Type)
  PatList   :: [AST Reduced (NT.Pattern NT.Type)]                   -> Maybe String  -> Node Reduced (NT.Pattern NT.Type)
  PatRecord :: [(String, Maybe § AST Reduced (NT.Pattern NT.Type))] -> Maybe String  -> Node Reduced (NT.Pattern NT.Type)
deriving instance Show (Node Reduced (NT.Pattern NT.Type))
deriving instance Show (AST Reduced (NT.Pattern NT.Type))
deriving instance Eq (Node Reduced (NT.Pattern NT.Type))
deriving instance Eq (AST Reduced (NT.Pattern NT.Type))
deriving instance Ord (Node Reduced (NT.Pattern NT.Type))
deriving instance Ord (AST Reduced (NT.Pattern NT.Type))




data Binding
  = Bind String TypeExpr
  | Constraint TypeConstraint
    deriving (Show, Eq, Ord)

type TypeConstraint = Node Reduced NT.Constraint
data instance Node Reduced NT.Constraint where
    Implements :: TypeExpr -> ProtocolID -> Node Reduced NT.Constraint
    Refinement :: Bindings -> Liquid  -> TypeExpr -> Node Reduced NT.Constraint
deriving instance Show (Node Reduced NT.Constraint)
deriving instance Show (AST Reduced NT.Constraint)
deriving instance Eq (Node Reduced NT.Constraint)
deriving instance Eq (AST Reduced NT.Constraint)
deriving instance Ord (Node Reduced NT.Constraint)
deriving instance Ord (AST Reduced NT.Constraint)

type Bindings = Map (Variable Liquid) TypeExpr
type ProtocolID = String

