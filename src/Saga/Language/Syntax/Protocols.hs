{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE UndecidableInstances #-}
module Saga.Language.Syntax.Protocols where

import           Prelude                                 hiding (id)


import qualified Data.Set                                as Set
import qualified Saga.Language.Syntax.AST                as NT (NodeType (..))
import           Saga.Language.Syntax.AST                hiding (NodeType (..))
import           Saga.Language.Syntax.Polymorphism       (Polymorphic)

import           Data.Data                               (Data)
import           Saga.Language.Syntax.Elaborated.Types   (Type)
import           Saga.Language.Syntax.Reduced.Types      (TypeExpr)
import           Saga.Language.Syntax.Reduced.Values     (Expr)
import           Saga.Language.Typechecker.Qualification (Qualified ((:=>)))
import           Saga.Language.Typechecker.Substitution  (Substitutable (..))


data Protocol phase = Protocol
  { id              :: ProtocolID
  , spec            :: Polymorphic (Node phase NT.Type)
  , implementations :: [Implementation phase]
  }
deriving instance (Show (Polymorphic (Node phase NT.Type)), Show (Implementation phase)) => (Show (Protocol phase))

instance Eq (Protocol phase) where
    p1 == p2 = id p1 == id p2


newtype Implementation phase = Implementation (Name, Node phase NT.Type, Node phase NT.Expression)

deriving instance (Show (Node phase NT.Type), Show (Node phase NT.Expression)) => (Show (Implementation phase))

instance Eq (Implementation phase) where
  Implementation (name, _, _) == Implementation (name', _, _) = name == name'
instance Ord (Implementation phase) where
  Implementation (name, _, _) `compare` Implementation (name', _, _) = compare name name'


type Method = (Name, TypeExpr)
newtype Name = Name String deriving (Show, Eq, Ord, Data)

type ProtocolID = String


