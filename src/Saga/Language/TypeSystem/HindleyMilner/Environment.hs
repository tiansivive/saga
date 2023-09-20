
{-# LANGUAGE MultiParamTypeClasses #-}

module Saga.Language.TypeSystem.HindleyMilner.Environment where

import           Control.Monad.Except
import           Control.Monad.State.Lazy                      hiding (get,
                                                                modify)
import qualified Control.Monad.State.Lazy                      as ST
import           Control.Monad.Trans.RWS                       (RWST, get,
                                                                local, modify,
                                                                tell)
import qualified Data.Map                                      as Map

import           Prelude                                       hiding (EQ)
import           Saga.Language.TypeSystem.HindleyMilner.Errors (SagaError)
import qualified Saga.Language.TypeSystem.HindleyMilner.Types  as T
import           Saga.Language.TypeSystem.HindleyMilner.Types  hiding
                                                               (Implements,
                                                                ProtocolID)
import           Saga.Lexer.Tokens                             (Token (Qualified))



type Saga t a = RWST () Accumulator CompilerState t a
data CompilerState = Saga 
  { protocols :: [Protocol]
  , values    :: Map.Map String TypeExpr
  , types     :: Map.Map String TypeExpr
  , kinds     :: Map.Map String Kind 
  } deriving (Show)

data Accumulator = Acc
  { logs:: [Log]
  , warnings:: [Warning]
  , errors:: [SagaError] 
  } deriving (Show)

data Tell = Log Log | Warning Warning | Error SagaError

log :: Monad m => Tell -> RWST r Accumulator s m ()
log (Log l)     = tell $ Acc { logs = [l], warnings=[], errors = [] }
log (Warning w) = tell $ Acc { logs = [], warnings=[w], errors = [] }
log (Error e)   = tell $ Acc { logs = [], warnings=[], errors = [e] }

instance Semigroup Accumulator where
  acc1 <> acc2 = Acc { logs = logs acc1 <> logs acc2, warnings = warnings acc1 <> warnings acc2, errors = errors acc1 <> errors acc2 }
instance Monoid Accumulator where
  mempty = Acc [] [] []
  mappend = (<>)


type Log = String
type Warning = String
type Error =  String


data Protocol = Protocol
  { id              :: ProtocolID
  , spec            :: TypeExpr
  , supers          :: [BaseProtocol]
  , implementations :: [Implementation]
  } deriving (Show)

-- type Infer = RWST InferenceEnv [IConstraint] InferState (Except InferenceError)

data InferenceEnv = Env
  { unificationVars :: Map.Map UnificationVar TypeExpr,
    aliases         :: Map.Map Alias TypeExpr
  }
  deriving (Show)

data IConstraint
  = Empty
  | EqCons Equality
  | ImplCons ImplConstraint
      -- \| Subtype Type Type -- Is this at all needed? probably not
      -- | Conjunction IConstraint IConstraint
      -- | Implication [UnificationVar] IConstraint IConstraint
  deriving (Show, Eq)

data Equality = EQ Type Type deriving (Show, Eq)
data ImplConstraint = IP Type String deriving (Show, Eq)




type UnificationVar = Tyvar

type TypeVar = String
type Alias = String
type Name = String
type BaseProtocol = String
type ProtocolID = String

type Implementation = Qualified ImplConstraint
type Method = (Name, TypeExpr)



data Scheme = Scheme [Tyvar] (Qualified Type) deriving (Show, Eq)
