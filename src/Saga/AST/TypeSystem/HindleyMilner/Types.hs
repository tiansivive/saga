{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module Saga.AST.TypeSystem.HindleyMilner.Types where
import           GHC.Base ()



data TypeExpr where
  Type            :: Type -> TypeExpr
  TIdentifier     :: String -> TypeExpr
  TParens         :: TypeExpr -> TypeExpr
  TConditional    :: TypeExpr -> TypeExpr -> TypeExpr -> TypeExpr
  TClause         :: [TypeExpr] -> TypeExpr -> TypeExpr
  TBlock          :: [TypeExpr] -> TypeExpr
  TReturn         :: TypeExpr -> TypeExpr
  TLambda         :: [String] -> TypeExpr -> TypeExpr
  TFnApp          :: TypeExpr -> [TypeExpr] -> TypeExpr

data Type where
  TLiteral          :: Term -> Type
  TPrimitive        :: BuiltInType -> Type
  TTuple            :: [TypeExpr] -> Type
  TRecord           :: [(String, TypeExpr)] -> Type
  TArrow            :: TypeExpr -> TypeExpr -> Type

  TParametric       :: [String] -> TypeExpr -> Type
  TVar              :: String -> Type

  TConstrained      :: [Constraint] -> TypeExpr -> Type
  TProtocol         :: TypeExpr -> Type
  TImplementation   :: TypeExpr -> [RequiredImplId] -> Type

  TUnit             :: Type


deriving instance Show TypeExpr
deriving instance Eq TypeExpr

deriving instance Show Type
deriving instance Eq Type


type RequiredImplId = String

data BuiltInType
    = TBool
    | TInt
    | TString
    deriving (Show, Eq)


data PolymorphicVar where
    TPolyVar :: Quantifier -> Multiplicity -> String -> PolymorphicVar

deriving instance Show PolymorphicVar
instance Eq PolymorphicVar where
    (TPolyVar _ _ id) ==  (TPolyVar _ _ id') = id == id'



data Expr where
  Term           :: Term -> Expr
  Identifier     :: String -> Expr
  Assign         :: String -> Expr -> Expr
  IfElse         :: Expr -> Expr -> Expr -> Expr
  Lambda         :: [String] -> Expr -> Expr
  FnApp          :: Expr -> [Expr] -> Expr
  Clause         :: [Expr] -> Expr -> Expr
  Block          :: [Expr] -> Expr
  Return         :: Expr -> Expr
  Parens         :: Expr -> Expr
  FieldAccess    :: Expr -> [String] -> Expr
data Term where
  LInt    :: Int -> Term
  LBool   :: Bool -> Term
  LString :: String -> Term
  LList   :: [Expr] -> Term
  LTuple  :: [Expr] -> Term
  LRecord :: [(String, Expr)] -> Term

deriving instance Show Expr
deriving instance Eq Expr

deriving instance Show Term
deriving instance Eq Term


data Constraint
  = Implements TypeExpr TypeExpr
  | Extends Type Type
    deriving ( Show, Eq)

data Quantifier = Forall | Exists
  deriving (Eq, Show)
data Multiplicity =
  Erased | Linear | Affine | Relevant | Custom Int
  deriving (Eq, Show)


data Mode = Strict | Lazy








