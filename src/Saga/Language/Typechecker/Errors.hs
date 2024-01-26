module Saga.Language.Typechecker.Errors where
import           Saga.Language.Core.Expr                      (Expr)
import           Saga.Language.Core.Literals                  (Literal)
import           Saga.Language.Typechecker.Kind               (Kind)
import           Saga.Language.Typechecker.Protocols          (ProtocolID)
import           Saga.Language.Typechecker.Refinement.Liquid  (Liquid, Op)
import           Saga.Language.Typechecker.Solver.Constraints (Constraint,
                                                               Evidence, Item)
import           Saga.Language.Typechecker.Type               (Polymorphic,
                                                               Type)
import           Saga.Language.Typechecker.TypeExpr           (TypeExpr)
import           Saga.Language.Typechecker.Variables          (Variable)

import qualified Saga.Language.Syntax.Desugared.Types         as DS
import qualified Saga.Language.Syntax.Evaluated.Types         as EV

data SagaError where
  UnboundVariable     :: String -> SagaError
  UndefinedIdentifier :: String-> SagaError

  UnexpectedType                      :: EV.Type -> String -> SagaError
  UnexpectedKind                      :: Kind -> String -> SagaError
  UnexpectedPolymorphicVariable       :: (Show a, Show (Variable a)) => Variable a -> SagaError
  UnexpectedInstantiationVariable     :: (Show a, Show (Variable a)) =>  Variable a -> SagaError
  UnexpectedVariable                  :: (Show a, Show (Variable a)) =>  Variable a -> SagaError

  UntypedInferredExpr :: Expr -> SagaError

  -- | TYPECHECKING
  PolymorphicToConcreteMismatch :: Polymorphic Type -> Polymorphic Type -> SagaError

  -- | REFINEMENTS
  UnexpectedLiquidNegation :: Liquid -> SagaError
  UnexpectedUnsimplifiedExpr :: Op -> Liquid -> Liquid -> SagaError


  -- | INSTANTIATION
  TooManyInstantiationArguments :: (Show t, Show (Variable t)) => Polymorphic t -> [t] -> SagaError

  -- | UNIFICATION
  UnificationMismatch :: [Type] -> [Type]-> SagaError
  UnificationFail     :: Type -> Type-> SagaError
  UnificationKindFail :: Kind -> Kind -> SagaError

  KindMismatch :: Kind -> Kind -> SagaError


  InfiniteType :: (Show a, Show (Variable a)) => (Variable a) -> Type-> SagaError
  InfiniteKind :: (Show a, Show (Variable a)) => Variable a -> Kind -> SagaError
  CircularKind :: Kind -> Kind -> SagaError

  RigidUnification :: (Show a, Show (Variable a)) => Variable a -> Type  -> SagaError

  -- | PROTOCOLS
  MissingProtocolImplementation :: ProtocolID -> Type -> SagaError
  MultipleImplementationEvidence :: Item -> ProtocolID -> SagaError
  EvidenceNotFound :: String -> SagaError
  UnexpectedEvidence :: Evidence -> String -> SagaError

  -- | REFINEMENTS
  UnsatisfiableRefinement :: Constraint -> SagaError

  -- | EVALUATION
  UnexpectedLocalPolymorphicType :: Polymorphic Type -> SagaError
  TooManyArguments :: DS.TypeExpr-> [DS.TypeExpr]-> SagaError

  SubtypeFailure  :: Type ->Type-> SagaError
  Fail            :: String-> SagaError

deriving instance Show SagaError



data Exception
  = forall a b. (Show a, Show b) => Unexpected a b
  | NotYetImplemented String
  | DivideByZero Literal Literal
  | forall a b c. (Show a, Show b, Show c) => EvalTypeError a b c


crash :: Exception -> b
crash = error . show

deriving instance Show Exception
