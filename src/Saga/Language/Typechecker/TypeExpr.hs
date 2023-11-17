module Saga.Language.Typechecker.TypeExpr where
import           Saga.Language.Core.Literals             (Literal)
import           Saga.Language.Typechecker.Kind          (Kind)
import qualified Saga.Language.Typechecker.Qualification as Q
import           Saga.Language.Typechecker.Variables     (PolymorphicVar)



data TypeExpr where
    Singleton     :: Literal -> TypeExpr
    Identifier    :: String -> TypeExpr

    Union         :: [TypeExpr] -> TypeExpr
    Tuple         :: [TypeExpr] -> TypeExpr
    Record        :: [(String, TypeExpr)] -> TypeExpr
    Arrow         :: TypeExpr -> TypeExpr -> TypeExpr

    Match            :: TypeExpr -> [Case] -> TypeExpr
    Clause           :: TypeExpr -> [Binding] -> TypeExpr
    Tagged           :: String -> TypeExpr -> TypeExpr

    Lambda           :: [String] -> TypeExpr -> TypeExpr
    Application      :: TypeExpr -> [TypeExpr] -> TypeExpr

    Implementation   :: ProtocolID -> TypeExpr -> TypeExpr

    KindedType        :: TypeExpr -> Kind -> TypeExpr



data Case
  = Case Pattern TypeExpr
  deriving (Show, Eq, Ord)

data Pattern
    = Wildcard
    | Id String
    | PatHole String
    | Lit Literal
    | PatArrow [Pattern]
    | PatTuple [Pattern] (Maybe String)
    | PatRecord [(String, Maybe Pattern)] (Maybe String)
    | PatData String [Pattern]
  deriving (Show, Eq, Ord)


data Binding
  = Bind String TypeExpr
  | Constraint (Q.Constraint TypeExpr)

  deriving (Show, Eq, Ord)

type ProtocolID = String

deriving instance Show TypeExpr
deriving instance Eq TypeExpr
deriving instance Ord TypeExpr

