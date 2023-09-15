{-# LANGUAGE MultiParamTypeClasses #-}

module Saga.Language.TypeSystem.HindleyMilner.Environment where

import           Control.Monad.Except
import           Control.Monad.State.Lazy                     hiding (get,
                                                               modify)
import qualified Control.Monad.State.Lazy                     as ST
import           Control.Monad.Trans.RWS                      (RWST, get, local,
                                                               modify, tell)
import qualified Data.Map                                     as Map
import           Prelude                                      hiding (EQ)
import qualified Saga.Language.TypeSystem.HindleyMilner.Types as T
import           Saga.Language.TypeSystem.HindleyMilner.Types hiding
                                                              (Implements,
                                                               ProtocolID)
import           Saga.Lexer.Tokens                            (Token (Qualified))


data Protocol = Protocol {id :: ProtocolID, spec :: [Method], supers :: [BaseProtocol], implementations :: [Implementation]}
  deriving (Show)

type Infer = RWST InferenceEnv [IConstraint] InferenceState (Except InferenceError)

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

data InferenceError
  = UnboundVariable String
  | UndefinedIdentifier Alias
  | UnexpectedType String
  | UnificationMismatch [Type] [Type]
  | UnificationFail Type Type
  | InfiniteType UnificationVar Type
  | SubtypeFailure Type Type
  | Fail String
  deriving (Show, Eq)


type UnificationVar = Tyvar

type TypeVar = String
type Alias = String
type Name = String
type BaseProtocol = String
type ProtocolID = String

type Implementation = Qualified ImplConstraint
type Method = (Name, TypeExpr)

newtype InferenceState = IST {count :: Int} deriving (Show)

data Scheme = Scheme [Tyvar] (Qualified Type) deriving (Show, Eq)
