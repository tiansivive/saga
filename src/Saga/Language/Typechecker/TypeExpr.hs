module Saga.Language.Typechecker.TypeExpr where
import           Saga.Language.Core.Literals             (Literal)
import           Saga.Language.Typechecker.Kind          (Kind)
import qualified Saga.Language.Typechecker.Qualification as Q
import           Saga.Language.Typechecker.Variables     (PolymorphicVar)



data TypeExpr where
    Atom             :: TypeAtom -> TypeExpr

    Match            :: TypeExpr -> [Case] -> TypeExpr
    Clause           :: TypeExpr -> [Binding] -> TypeExpr
    Tagged           :: String -> TypeExpr -> TypeExpr

    Lambda           :: [PolymorphicVar TypeExpr] -> TypeExpr -> TypeExpr
    Application      :: TypeExpr -> [TypeExpr] -> TypeExpr

    Implementation   :: ProtocolID -> TypeExpr -> TypeExpr

    KindedType        :: TypeExpr -> Kind -> TypeExpr


data TypeAtom where
  Union         :: [TypeExpr] -> TypeAtom
  Tuple         :: [TypeExpr] -> TypeAtom
  Record        :: [(String, TypeExpr)] -> TypeAtom
  Arrow         :: TypeExpr -> TypeExpr -> TypeAtom
  Identifier    :: String -> TypeAtom

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

deriving instance Show TypeAtom
deriving instance Eq TypeAtom
deriving instance Ord TypeAtom

deriving instance Show TypeExpr
deriving instance Eq TypeExpr
deriving instance Ord TypeExpr
