{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Typechecker.Env where

    
import Saga.Language.Syntax.Protocols (Protocol) 
import Saga.Language.Typechecker.Errors (SagaError)

import Data.Map (Map)
import Saga.Language.Syntax.Elaborated.AST
import Saga.Language.Syntax.Elaborated.Types

import           Saga.Language.Syntax.AST hiding (NodeType (..))
import qualified Saga.Language.Syntax.AST as NT (NodeType (..))

import qualified Saga.Language.Syntax.Reduced.Values as RD

data CompilerState (phase :: Phase) = Saga
  { protocols   :: [Protocol phase]
  , values      :: Map String (AST phase NT.Expression)
  , types       :: Map String (AST phase NT.Type)
  , kinds       :: Map String (AST phase NT.Kind)
  , extra       :: ExtraData phase
  }
deriving instance 
  ( Show (Protocol phase)
  , Show (AST phase NT.Expression)
  , Show (AST phase NT.Type)
  , Show (AST phase NT.Kind)
  , Show (ExtraData phase)
  ) => (Show (CompilerState phase))


data Info = Info
  { logs:: [Log]
  , warnings:: [Warning]
  , errors:: [SagaError] 
  } 
  deriving (Show)

type family ExtraData (phase :: Phase) where
  ExtraData Elaborated = Proofs
  ExtraData Reduced = ()

data Proofs = Proofs { scrutinee :: Node Elaborated NT.Type, narrowings :: Map Type Type }
  deriving (Show)

type Log = String
type Warning = String
type Error =  String

instance Semigroup Info where
  acc1 <> acc2 = Info { logs = logs acc1 <> logs acc2, warnings = warnings acc1 <> warnings acc2, errors = errors acc1 <> errors acc2 }
instance Monoid Info where
  mempty = Info [] [] []
  mappend = (<>)
