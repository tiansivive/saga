module Saga.Language.Typechecker.Errors where
import           Saga.Language.Typechecker.Kind      (Kind)
import           Saga.Language.Typechecker.Type      (Polymorphic, Tag, Type)
import           Saga.Language.Typechecker.TypeExpr  (TypeExpr)
import           Saga.Language.Typechecker.Variables (PolymorphicVar)

data SagaError where
  UnboundVariable :: String -> SagaError
  UndefinedIdentifier :: String-> SagaError

  UnexpectedType :: Type -> String -> SagaError
  UnexpectedKind :: Kind -> String -> SagaError
  UnexpectedPolymorphicVariable :: Show a => PolymorphicVar a -> SagaError
  UnexpectedInstantiationVariable :: Show a =>  PolymorphicVar a -> SagaError

  TagNotConstructor       :: String -> SagaError
  MultipleTagConstructors :: [Tag] -> SagaError

  -- | INSTANTIATION
  TooManyInstantiationArguments :: Show t => Polymorphic t -> [t] -> SagaError

  -- | UNIFICATION
  UnificationMismatch :: [Type] -> [Type]-> SagaError
  UnificationFail :: Type -> Type-> SagaError
  UnificationKindFail :: Kind -> Kind -> SagaError

  KindMismatch :: Kind -> Kind -> SagaError

  InfiniteType :: (Show a) => (PolymorphicVar a) -> Type-> SagaError
  InfiniteKind :: (Show a) => PolymorphicVar a -> Kind -> SagaError

  -- | EVALUATION
  TooManyArguments :: TypeExpr-> [TypeExpr]-> SagaError

  SubtypeFailure :: Type ->Type-> SagaError
  Fail :: String-> SagaError

deriving instance Show SagaError
