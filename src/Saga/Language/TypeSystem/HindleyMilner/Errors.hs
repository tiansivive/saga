module Saga.Language.TypeSystem.HindleyMilner.Errors where

import           Saga.Language.TypeSystem.HindleyMilner.Types (Type, TypeExpr,
                                                               Tyvar)

data SagaError
  = UnboundVariable String
  | UndefinedIdentifier String
  | UnexpectedType String
  | UnificationMismatch [Type] [Type]
  | UnificationFail Type Type
  | InfiniteType Tyvar Type
  | SubtypeFailure Type Type
  | Fail String
  | TooManyArguments TypeExpr [TypeExpr]
  deriving (Show, Eq)
