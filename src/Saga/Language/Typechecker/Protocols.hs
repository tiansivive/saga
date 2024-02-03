{-# LANGUAGE DataKinds #-}
module Saga.Language.Typechecker.Protocols where

import           Prelude                                 hiding (id)


import qualified Data.Set                                as Set
import qualified Saga.Language.Syntax.AST                as NT (NodeType (..))
import           Saga.Language.Syntax.AST                hiding (NodeType (..))
import           Saga.Language.Syntax.Polymorphism       (Polymorphic)

import           Saga.Language.Syntax.Elaborated.Types   (Type)
import           Saga.Language.Syntax.Reduced.Types      (TypeExpr)
import           Saga.Language.Syntax.Reduced.Values     (Expr)
import           Saga.Language.Typechecker.Qualification (Qualified ((:=>)))
import           Saga.Language.Typechecker.Substitution  (Substitutable (..))


data Protocol = Protocol
  { id              :: ProtocolID
  , spec            :: Polymorphic Type
  , implementations :: [Implementation]
  } deriving (Show)

instance Eq Protocol where
    p1 == p2 = id p1 == id p2


newtype Implementation = Implementation (Name, Type, Expr) deriving Show
instance Eq Implementation where
  Implementation (name, _, _) == Implementation (name', _, _) = name == name'
instance Ord Implementation where
  Implementation (name, _, _) `compare` Implementation (name', _, _) = compare name name'


type Method = (Name, TypeExpr)
newtype Name = Name String deriving (Show, Eq, Ord)

type ProtocolID = String


