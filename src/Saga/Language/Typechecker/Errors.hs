module Saga.Language.Typechecker.Errors where
import           Saga.Language.Typechecker.Kind            (Kind)
import           Saga.Language.Typechecker.TypeExpr (TypeExpr)
import           Saga.Language.Typechecker.Type            (Type)
import           Saga.Language.Typechecker.Variables       (PolymorphicVar)

data SagaError where
  UnboundVariable :: String -> SagaError
  UndefinedIdentifier :: String-> SagaError
  UnexpectedType :: Type -> String -> SagaError
  UnificationMismatch :: [Type] -> [Type]-> SagaError
  UnificationFail :: Type -> Type-> SagaError
  UnificationKindFail :: Kind -> Kind-> SagaError
  InfiniteType :: (Show a) => (PolymorphicVar a) -> Type-> SagaError
  InfiniteKind :: String ->Kind-> SagaError
  SubtypeFailure :: Type ->Type-> SagaError
  Fail :: String-> SagaError
  TooManyArguments :: TypeExpr-> [TypeExpr]-> SagaError

deriving instance Show SagaError
