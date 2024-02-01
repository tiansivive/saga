{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Syntax.Desugared.Types where

import qualified Saga.Language.Syntax.AST             as NT (NodeType (..))
import           Saga.Language.Syntax.AST             hiding (NodeType (..))

import           Saga.Language.Syntax.Desugared.AST
import           Saga.Language.Syntax.Desugared.Kinds
import           Saga.Language.Syntax.Liquids

import           Data.Map                             (Map)
import           Saga.Language.Syntax.Literals

import           Saga.Language.Typechecker.Variables  (Variable)
import           Saga.Utils.TypeLevel                 (type (§))


type TypeExpr = Node Desugared NT.Type
type ExprAST = AST Desugared NT.Type

data instance Node Desugared NT.Type where
    Singleton     :: Literal -> TypeExpr
    Identifier    :: String  -> TypeExpr

    Union         :: [TypeExpr] -> TypeExpr
    Tuple         :: [TypeExpr] -> TypeExpr
    Record        :: [(String, TypeExpr)] -> TypeExpr
    Arrow         :: TypeExpr -> TypeExpr -> TypeExpr

    Match            :: TypeExpr -> [Node Desugared (NT.Case NT.Type)] -> TypeExpr
    Clause           :: TypeExpr -> [Binding] -> TypeExpr
    Tagged           :: String -> TypeExpr -> TypeExpr

    Lambda           :: [String] -> TypeExpr -> TypeExpr
    Application      :: TypeExpr -> [TypeExpr] -> TypeExpr

    Implementation   :: ProtocolID -> TypeExpr -> TypeExpr
deriving instance Show TypeExpr
deriving instance Show (AST Desugared NT.Type)
deriving instance Eq TypeExpr
deriving instance Eq (AST Desugared NT.Type)
deriving instance Ord TypeExpr
deriving instance Ord (AST Desugared NT.Type)

data instance Node Desugared (NT.Case NT.Type) where
  Case :: AST Desugared (NT.Pattern NT.Type) -> AST Desugared (NT.Case NT.Type) -> Node Desugared (NT.Case NT.Type)
deriving instance Show (Node Desugared (NT.Case NT.Type))
deriving instance Show (AST Desugared (NT.Case NT.Type))
deriving instance Eq (Node Desugared (NT.Case NT.Type))
deriving instance Eq (AST Desugared (NT.Case NT.Type))
deriving instance Ord (Node Desugared (NT.Case NT.Type))
deriving instance Ord (AST Desugared (NT.Case NT.Type))

data instance Node Desugared (NT.Pattern NT.Type) where
  Wildcard  :: Node Desugared (NT.Pattern NT.Type)
  Id        :: String                                                               -> Node Desugared (NT.Pattern NT.Type)
  PatHole   :: String                                                               -> Node Desugared (NT.Pattern NT.Type)
  PatLit    :: Literal                                                              -> Node Desugared (NT.Pattern NT.Type)
  PatArrow  :: [AST Desugared (NT.Pattern NT.Type)]                                    -> Node Desugared (NT.Pattern NT.Type)
  PatTuple  :: [AST Desugared (NT.Pattern NT.Type)]                   -> Maybe String  -> Node Desugared (NT.Pattern NT.Type)
  PatList   :: [AST Desugared (NT.Pattern NT.Type)]                   -> Maybe String  -> Node Desugared (NT.Pattern NT.Type)
  PatRecord :: [(String, Maybe § AST Desugared (NT.Pattern NT.Type))] -> Maybe String  -> Node Desugared (NT.Pattern NT.Type)
deriving instance Show (Node Desugared (NT.Pattern NT.Type))
deriving instance Show (AST Desugared (NT.Pattern NT.Type))
deriving instance Eq (Node Desugared (NT.Pattern NT.Type))
deriving instance Eq (AST Desugared (NT.Pattern NT.Type))
deriving instance Ord (Node Desugared (NT.Pattern NT.Type))
deriving instance Ord (AST Desugared (NT.Pattern NT.Type))




data Binding
  = Bind String TypeExpr
  | Constraint (Node Desugared NT.Constraint)
    deriving (Show, Eq, Ord)

type TypeConstraint = Node Desugared NT.Constraint
data instance Node Desugared NT.Constraint where
    Implements :: TypeExpr -> ProtocolID -> Node Desugared NT.Constraint
    Refinement :: Bindings -> Liquid  -> TypeExpr -> Node Desugared NT.Constraint
deriving instance Show (Node Desugared NT.Constraint)
deriving instance Show (AST Desugared NT.Constraint)
deriving instance Eq (Node Desugared NT.Constraint)
deriving instance Eq (AST Desugared NT.Constraint)
deriving instance Ord (Node Desugared NT.Constraint)
deriving instance Ord (AST Desugared NT.Constraint)

type Bindings = Map (Variable Liquid) TypeExpr
type ProtocolID = String
