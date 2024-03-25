{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Syntax.Elaborated.Types where

import           Data.Map                                        (Map)
import qualified Saga.Language.Syntax.AST                        as NT (NodeType (..))
import           Saga.Language.Syntax.AST                        hiding
                                                                 (NodeType (..))
import           Saga.Language.Syntax.Elaborated.AST
import           Saga.Language.Syntax.Polymorphism

import           Saga.Language.Syntax.Literals

import           Saga.Language.Syntax.Elaborated.Kinds           hiding (Arrow,
                                                                  Polymorphic,
                                                                  Qualified,
                                                                  Var)
import           Saga.Language.Syntax.Liquids                    hiding (Var)
import           Saga.Language.Typechecker.Variables             (Variable)

import           Control.Monad.Identity                          (Identity)
import           Data.Data                                       (Data)
import qualified Data.Kind                                       as GHC
import           Documentation.SBV.Examples.BitPrecise.MergeSort (E)
import           Prelude                                         hiding (map,
                                                                  traverse)
import           Saga.Language.Syntax.Reduced.Types              (TypeExpr)
import           Saga.Utils.Common                               (fmap2, mapM2)
import           Saga.Utils.TypeLevel                            (type (§))

type Type = Node Elaborated NT.Type
data instance Node Elaborated NT.Type where
    Singleton   :: Literal                                              -> Type
    Data        :: String                                               -> Type
    Var         :: Variable Type                                        -> Type
    Tuple       :: [AST Elaborated NT.Type]                             -> Type
    Record      :: [(String, AST Elaborated NT.Type)]                   -> Type
    Arrow       :: AST Elaborated NT.Type -> AST Elaborated NT.Type     -> Type
    Union       :: [AST Elaborated NT.Type]                             -> Type
   -- Closure     :: [String] -> TypeExpr -> Scope                 -> Type
    -- Lambda    :: String -> AST Elaborated NT.Type                   -> Type
    Match       :: { subject :: AST Elaborated NT.Type, cases :: [AST Elaborated (NT.Case NT.Type)] } -> Type
    Polymorphic :: Polymorphic Type                                     -> Type
    Qualified   :: Qualified Type                                       -> Type
    Applied     :: AST Elaborated NT.Type -> AST Elaborated NT.Type     -> Type
    Void        :: Type
    Any         :: Type



deriving instance Show Type
deriving instance Eq Type
deriving instance Ord Type
deriving instance Data Type

deriving instance Show (AST Elaborated NT.Type)
deriving instance Eq (AST Elaborated NT.Type)
deriving instance Ord (AST Elaborated NT.Type)
deriving instance Data (AST Elaborated NT.Type)

data Scope = Scope
  { types :: Map String (AST Elaborated NT.Type)
  , kinds :: Map String (AST Elaborated NT.Kind)
  } deriving (Show, Eq, Ord, Data)


data instance Variable Type where
  Poly              :: String -> Kind -> Variable Type
  Existential       :: String -> Kind -> Variable Type
  Local             :: String -> Kind -> Variable Type
  Skolem            :: String -> Kind -> Variable Type
  Rigid             :: String -> Kind -> Variable Type
  Scoped            :: String -> Kind -> Variable Type
  Unification       :: String -> Kind -> Variable Type
  Evaluation        :: String -> Kind -> Variable Type
  Instantiation     :: String -> Kind -> Variable Type
deriving instance Show (Variable Type)
deriving instance Eq (Variable Type)
deriving instance Ord (Variable Type)
deriving instance Data (Variable Type)

data instance Node Elaborated (NT.Case NT.Type) where
  Case :: AST Elaborated (NT.Pattern NT.Type) -> AST Elaborated NT.Type -> Node Elaborated (NT.Case NT.Type)
deriving instance Show (Node Elaborated (NT.Case NT.Type))
deriving instance Show (AST Elaborated (NT.Case NT.Type))
deriving instance Eq (Node Elaborated (NT.Case NT.Type))
deriving instance Eq (AST Elaborated (NT.Case NT.Type))
deriving instance Ord (Node Elaborated (NT.Case NT.Type))
deriving instance Ord (AST Elaborated (NT.Case NT.Type))
deriving instance Data (Node Elaborated (NT.Case NT.Type))
deriving instance Data (AST Elaborated (NT.Case NT.Type))

data instance Node Elaborated (NT.Pattern NT.Type) where
  Wildcard  :: Node Elaborated (NT.Pattern NT.Type)
  Id        :: String -> Node Elaborated (NT.Pattern NT.Type)
  PatHole   :: String -> Node Elaborated (NT.Pattern NT.Type)
  PatLit    :: Literal  -> Node Elaborated (NT.Pattern NT.Type)
  PatTuple  :: [AST Elaborated (NT.Pattern NT.Type)] -> Maybe String  -> Node Elaborated (NT.Pattern NT.Type)
  PatRecord :: [(String, AST Elaborated (NT.Pattern NT.Type))] -> Maybe String  -> Node Elaborated (NT.Pattern NT.Type)
  PatData   :: String -> [AST Elaborated (NT.Pattern NT.Type)]  -> Node Elaborated (NT.Pattern NT.Type)
deriving instance Show (Node Elaborated (NT.Pattern NT.Type))
deriving instance Show (AST Elaborated (NT.Pattern NT.Type))
deriving instance Eq (Node Elaborated (NT.Pattern NT.Type))
deriving instance Eq (AST Elaborated (NT.Pattern NT.Type))
deriving instance Ord (Node Elaborated (NT.Pattern NT.Type))
deriving instance Ord (AST Elaborated (NT.Pattern NT.Type))
deriving instance Data (Node Elaborated (NT.Pattern NT.Type))
deriving instance Data (AST Elaborated (NT.Pattern NT.Type))



type TypeConstraint = Node Elaborated NT.Constraint
data instance Node Elaborated NT.Constraint where
  Implements :: Type -> ProtocolID -> Node Elaborated NT.Constraint
  Refinement :: Bindings -> Liquid  -> Type -> Node Elaborated NT.Constraint


type Bindings = Map (Variable Liquid) Type
type ProtocolID = String

deriving instance Show TypeConstraint
deriving instance Eq TypeConstraint
deriving instance Ord TypeConstraint
deriving instance Data TypeConstraint

deriving instance Show (AST Elaborated NT.Constraint)

type instance Qualifier Type = TypeConstraint

