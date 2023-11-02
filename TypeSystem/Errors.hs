module Saga.Language.TypeSystem.Errors where

import           Saga.Language.TypeSystem.Types (Kind, Type, TypeExpr, Tyvar)

data SagaError
  = UnboundVariable String
  | UndefinedIdentifier String
  | UnexpectedType String
  | UnificationMismatch [Type] [Type]
  | UnificationFail Type Type
  | UnificationKindFail Kind Kind
  | InfiniteType Tyvar Type
  | InfiniteKind String Kind
  | SubtypeFailure Type Type
  | Fail String
  | TooManyArguments TypeExpr [TypeExpr]
  deriving (Show, Eq)
