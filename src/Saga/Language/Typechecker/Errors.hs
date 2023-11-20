module Saga.Language.Typechecker.Errors where
import           Saga.Language.Core.Expr                      (Expr)
import           Saga.Language.Typechecker.Kind               (Kind)
import           Saga.Language.Typechecker.Protocols          (ProtocolID)
import           Saga.Language.Typechecker.Refinement.Liquid  (Liquid)
import           Saga.Language.Typechecker.Solver.Constraints (Evidence, Item)
import           Saga.Language.Typechecker.Type               (Polymorphic, Tag,
                                                               Type)
import           Saga.Language.Typechecker.TypeExpr           (TypeExpr)
import           Saga.Language.Typechecker.Variables          (PolymorphicVar)

data SagaError where
  UnboundVariable     :: String -> SagaError
  UndefinedIdentifier :: String-> SagaError



  UnexpectedType                      :: Type -> String -> SagaError
  UnexpectedKind                      :: Kind -> String -> SagaError
  UnexpectedPolymorphicVariable       :: Show a => PolymorphicVar a -> SagaError
  UnexpectedInstantiationVariable     :: Show a =>  PolymorphicVar a -> SagaError
  UnexpectedVariable                  :: Show a =>  PolymorphicVar a -> SagaError

  TagNotConstructor       :: String -> SagaError
  MultipleTagConstructors :: [Tag] -> SagaError

  UntypedInferredExpr :: Expr -> SagaError

  -- | REFINEMENTS
  UnexpectedLiquidNegation :: Liquid -> SagaError

  -- | INSTANTIATION
  TooManyInstantiationArguments :: Show t => Polymorphic t -> [t] -> SagaError

  -- | UNIFICATION
  UnificationMismatch :: [Type] -> [Type]-> SagaError
  UnificationFail     :: Type -> Type-> SagaError
  UnificationKindFail :: Kind -> Kind -> SagaError

  KindMismatch :: Kind -> Kind -> SagaError


  InfiniteType :: (Show a) => (PolymorphicVar a) -> Type-> SagaError
  InfiniteKind :: (Show a) => PolymorphicVar a -> Kind -> SagaError
  CircularKind :: Kind -> Kind -> SagaError

  -- | PROTOCOLS
  MissingProtocolImplementation :: ProtocolID -> Type -> SagaError
  MultipleImplementationEvidence :: Item -> ProtocolID -> SagaError
  EvidenceNotFound :: String -> SagaError
  UnexpectedEvidence :: Evidence -> String -> SagaError

  -- | EVALUATION
  UnexpectedLocalPolymorphicType :: Polymorphic Type -> SagaError
  TooManyArguments :: TypeExpr-> [TypeExpr]-> SagaError

  SubtypeFailure  :: Type ->Type-> SagaError
  Fail            :: String-> SagaError

deriving instance Show SagaError



data Exception
  = forall a b. (Show a, Show b) => Unexpected a b
  | NotYetImplemented String


crash :: Exception -> b
crash = error . show

deriving instance Show Exception
