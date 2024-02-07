module Saga.Language.Typechecker.Errors where


import           Saga.Language.Typechecker.Variables           (Variable)


import qualified Saga.Language.Syntax.AST                      as NT (NodeType (..))
import           Saga.Language.Syntax.AST                      (AST, Phase (..))
import qualified Saga.Language.Syntax.Elaborated.Kinds         as EL
import qualified Saga.Language.Syntax.Elaborated.Types         as EL
import           Saga.Language.Syntax.Liquids                  (Liquid, Op)
import           Saga.Language.Syntax.Literals                 (Literal)
import           Saga.Language.Syntax.Polymorphism             (Polymorphic)
import           Saga.Language.Syntax.Protocols                (ProtocolID)
import qualified Saga.Language.Syntax.Reduced.Types            as RD
import qualified Saga.Language.Syntax.Reduced.Values           as RD
import qualified Saga.Language.Typechecker.Solving.Constraints as Solver

data SagaError where
  UnboundVariable     :: String -> SagaError
  UndefinedIdentifier :: String-> SagaError

  UnexpectedType                      :: RD.TypeExpr -> String -> SagaError
  UnexpectedKind                      :: EL.Kind -> String -> SagaError
  UnexpectedPolymorphicVariable       :: (Show a, Show (Variable a)) => Variable a -> SagaError
  UnexpectedInstantiationVariable     :: (Show a, Show (Variable a)) =>  Variable a -> SagaError
  UnexpectedVariable                  :: (Show a, Show (Variable a)) =>  Variable a -> SagaError

  UntypedInferredExpr :: AST Elaborated NT.Expression -> SagaError

  -- | EL.TypeCHECKING
  PolymorphicToConcreteMismatch :: Polymorphic EL.Type -> Polymorphic EL.Type -> SagaError

  -- | REFINEMENTS
  UnexpectedLiquidNegation :: Liquid -> SagaError
  UnexpectedUnsimplifiedExpr :: Op -> Liquid -> Liquid -> SagaError


  -- | INSTANTIATION
  TooManyInstantiationArguments :: (Show t, Show (Variable t)) => Polymorphic t -> [t] -> SagaError

  -- | UNIFICATION
  UnificationMismatch :: [EL.Type] -> [EL.Type]-> SagaError
  UnificationFail     :: EL.Type -> EL.Type-> SagaError
  UnificationKindFail :: EL.Kind -> EL.Kind -> SagaError

  KindMismatch :: EL.Kind -> EL.Kind -> SagaError


  InfiniteType :: (Show a, Show (Variable a)) => (Variable a) -> EL.Type -> SagaError
  InfiniteKind :: (Show a, Show (Variable a)) => Variable a -> EL.Kind -> SagaError
  CircularKind :: EL.Kind -> EL.Kind -> SagaError

  RigidUnification :: (Show a, Show (Variable a), Show (EL.Node Elaborated nt)) => Variable a -> EL.Node Elaborated nt  -> SagaError

  -- | PROTOCOLS
  MissingProtocol :: ProtocolID -> SagaError
  MissingProtocolImplementation :: ProtocolID -> EL.Type -> SagaError
  MultipleImplementationEvidence :: EL.Type -> ProtocolID -> SagaError
  EvidenceNotFound :: String -> SagaError
  UnexpectedEvidence :: Solver.Evidence -> String -> SagaError

  -- | REFINEMENTS
  UnsatisfiableRefinement :: Solver.Constraint -> SagaError

  -- | EVALUATION
  UnexpectedLocalPolymorphicType :: Polymorphic EL.Type -> SagaError
  TooManyArguments :: RD.TypeExpr-> [RD.TypeExpr]-> SagaError

  SubtypeFailure  :: EL.Type -> EL.Type -> SagaError
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
