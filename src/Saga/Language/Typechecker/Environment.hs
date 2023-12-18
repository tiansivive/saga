{-# LANGUAGE TypeFamilies #-}


module Saga.Language.Typechecker.Environment where
import           Control.Monad.RWS
import qualified Data.Map                            as Map
import qualified Saga.Language.Core.Expr           as Value
import           Saga.Language.Typechecker.Kind      (Kind)
import           Saga.Language.Typechecker.Protocols
import           Saga.Language.Typechecker.Type      (Polymorphic, Type, DataType, Tag)

import Saga.Language.Typechecker.Variables
import Saga.Language.Core.Expr (Expr)
import Saga.Language.Typechecker.TypeExpr (TypeExpr, Pattern, Case)
import Saga.Language.Typechecker.Solver.Constraints (Assumption)
import Saga.Language.Typechecker.Errors (SagaError)

type Saga = MonadRWS Config Info CompilerState 
newtype Config = Config { init:: Map.Map String String }

data CompilerState = Saga
  { protocols   :: [Protocol]
  , values      :: Map.Map String Value.Expr
  , types       :: Map.Map String (Polymorphic Type)
  , kinds       :: Map.Map String (Polymorphic Kind)
, dataTypes   :: Map.Map String DataType
  , tags        :: [Tag]
  , assumptions :: [Assumption]
  } deriving (Show)




data Info = Info
  { logs:: [Log]
  , warnings:: [Warning]
  , errors:: [SagaError] 
  } 
  deriving (Show)

type Log = String
type Warning = String
type Error =  String

instance Semigroup Info where
  acc1 <> acc2 = Info { logs = logs acc1 <> logs acc2, warnings = warnings acc1 <> warnings acc2, errors = errors acc1 <> errors acc2 }
instance Monoid Info where
  mempty = Info [] [] []
  mappend = (<>)


type instance Classifier Expr     = Type
type instance Classifier Pattern  = Type
type instance Classifier Case     = Type
type instance Classifier Type     = Kind
type instance Classifier TypeExpr = Kind
type instance Classifier Kind     = Kind

