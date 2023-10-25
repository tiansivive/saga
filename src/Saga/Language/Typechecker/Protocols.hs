module Saga.Language.Typechecker.Protocols where

import           Prelude                                 hiding (id)
import           Saga.Language.Core.Expr                 (Expr)
import           Saga.Language.Typechecker.Kind          (Kind)
import           Saga.Language.Typechecker.Qualification (Qualified)
import           Saga.Language.Typechecker.Type          (Polymorphic, Type)
import           Saga.Language.Typechecker.TypeExpr      (TypeExpr)

data Protocol = Protocol
  { id              :: ProtocolID
  , spec            :: TypeExpr
  , supers          :: [BaseProtocol]
  , implementations :: [Implementation]
  } deriving (Show)

instance Eq Protocol where
    p1 == p2 = id p1 == id p2

type Implementation = Qualified (Name, Type, Expr)
type Method = (Name, TypeExpr )

newtype Name = Name String deriving (Show, Eq)

type ProtocolID = String
type BaseProtocol = String

