{-# LANGUAGE DataKinds #-}
module Saga.Language.Typechecker.Protocols where

import           Prelude                                       hiding (id)


import qualified Data.Set                                      as Set
import           Saga.Language.Typechecker.Qualification       (Qualified ((:=>)))
import           Saga.Language.Typechecker.Solver.Substitution (Substitutable (..))
import           Saga.Language.Typechecker.Type                (Polymorphic,
                                                                Type)
import           Saga.Language.Typechecker.TypeExpr            (TypeExpr)

import qualified Saga.Language.Core.AST                        as AST

data Protocol = Protocol
  { id              :: ProtocolID
  , spec            :: TypeExpr
  , implementations :: [Implementation]
  } deriving (Show)

instance Eq Protocol where
    p1 == p2 = id p1 == id p2


data Implementation = forall (phase :: AST.Phase). Implementation (Name, Polymorphic Type, AST.Node phase AST.Expression)

deriving instance Show Implementation
instance Eq Implementation where
  Implementation (name, _, _) == Implementation (name', _, _) = name == name'
instance Ord Implementation where
  Implementation (name, _, _) `compare` Implementation (name', _, _) = compare name name'


type Method = (Name, TypeExpr)
newtype Name = Name String deriving (Show, Eq, Ord)

type ProtocolID = String


