{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Typechecker.Env where

    
import Saga.Language.Typechecker.Protocols (Protocol) 
import Saga.Language.Typechecker.Errors (SagaError)

import Data.Map (Map)
import Saga.Language.Syntax.Elaborated.AST
import Saga.Language.Syntax.Elaborated.Types

import Saga.Language.Syntax.AST
import qualified Saga.Language.Syntax.AST as NT (NodeType (..))

import qualified Saga.Language.Syntax.Desugared.Values as Desugared

data CompilerState (phase :: Phase) = Saga
  { protocols   :: [Protocol]
  , values      :: Map String Desugared.Expr
  , types       :: Map String (AST phase NT.Type)
  , kinds       :: Map String (AST phase NT.Kind)
  , extra       :: ExtraData phase
  }
deriving instance (Show (AST phase NT.Type), Show (AST phase NT.Kind), Show (ExtraData phase)) => (Show (CompilerState phase))


data Info = Info
  { logs:: [Log]
  , warnings:: [Warning]
  , errors:: [SagaError] 
  } 
  deriving (Show)

type family ExtraData (phase :: Phase) where
  ExtraData 'Desugared = ()
  ExtraData 'Elaborated = Proofs
  ExtraData 'Evaluated = ()

data Proofs = Proofs { scrutinee :: Node 'Elaborated NT.Type, narrowings :: Map Type Type }
  deriving (Show)

type Log = String
type Warning = String
type Error =  String

instance Semigroup Info where
  acc1 <> acc2 = Info { logs = logs acc1 <> logs acc2, warnings = warnings acc1 <> warnings acc2, errors = errors acc1 <> errors acc2 }
instance Monoid Info where
  mempty = Info [] [] []
  mappend = (<>)
