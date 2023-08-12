{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module Saga.AST.TypeSystem.HindleyMilner.Types where

import           GHC.Base          ()
import           Saga.Lexer.Tokens (Token (Qualified))

data TypeExpr where
  TTerm :: Term -> TypeExpr
  TIdentifier :: String -> TypeExpr
  TETuple :: [TypeExpr] -> TypeExpr
  TERecord :: [(String, TypeExpr)] -> TypeExpr
  TEArrow :: TypeExpr -> TypeExpr -> TypeExpr
  TParens :: TypeExpr -> TypeExpr
  TConditional :: TypeExpr -> TypeExpr -> TypeExpr -> TypeExpr
  TClause      :: TypeExpr -> [Binding] -> TypeExpr
  -- TBlock          :: [TypeExpr] -> TypeExpr
  -- TReturn         :: TypeExpr -> TypeExpr
  TLambda :: [String] -> TypeExpr -> TypeExpr
  TFnApp :: TypeExpr -> [TypeExpr] -> TypeExpr
  TImplementation :: ProtocolId -> TypeExpr -> TypeExpr
  TQuantified :: Quantifier -> TypeExpr -> TypeExpr
  TConstraint :: [Constraint] -> TypeExpr -> TypeExpr


data Binding
  = Bind String TypeExpr
  | ImplBind String [TypeExpr]
  | SubtypeBind String TypeExpr
  | RefineBind String TypeExpr
  deriving (Show, Eq)



data Type where
  TLiteral :: Term -> Type
  TPrimitive :: BuiltInType -> Type
  TTuple :: [Type] -> Type
  TRecord :: [(String, Type)] -> Type
  TArrow :: Type -> Type -> Type
  TConstructor :: Tycon -> Type
  TParametric :: Type -> TypeExpr -> Type
  TVar :: Tyvar -> Type
  -- TConstrained :: [Constraint] -> Type -> Type
  -- TProtocol         :: TypeExpr -> Type
  -- TImplementation   :: TypeExpr -> [RequiredImplId] -> Type

  TUnit :: Type

type ProtocolId = String
data Tycon = Tycon String Kind deriving (Show, Eq)
data Tyvar = Tyvar String Kind deriving (Show, Eq, Ord)
data BuiltInType
  = TBool
  | TInt
  | TString
  deriving (Show, Eq)


deriving instance Show TypeExpr
deriving instance Eq TypeExpr
deriving instance Show Type
deriving instance Eq Type



infixr 5 `TArrow`
infixr 0 :=>
data Qualified t = (:=>) { constraints :: [Constraint], ty:: t } --, mode :: Mode, multiplicity :: Multiplicity }
  deriving (Show, Eq)

data Constraint
  = Type `Implements` String
  -- | Extends Type Type
  deriving (Show, Eq)



data Kind
  = KType
  | KConstructor Kind Kind
  | KConstraint
  | KProtocol
  | KVar String
    deriving (Show, Eq, Ord)



data Quantifier = Forall | Exists
  deriving (Eq, Show)

data Multiplicity
  = Erased
  | Linear
  | Affine
  | Relevant
  | Custom Int
  deriving (Eq, Show)

data Mode = Strict | Lazy






-- | Term expressions
data Expr where
  Term :: Term -> Expr
  Identifier :: String -> Expr
  List :: [Expr] -> Expr
  Tuple :: [Expr] -> Expr
  Record :: [(String, Expr)] -> Expr
  Assign :: String -> Expr -> Expr
  IfElse :: Expr -> Expr -> Expr -> Expr
  Lambda :: [String] -> Expr -> Expr
  FnApp :: Expr -> [Expr] -> Expr
  Clause :: [Expr] -> Expr -> Expr
  Block :: [Expr] -> Expr
  Return :: Expr -> Expr
  Parens :: Expr -> Expr
  FieldAccess :: Expr -> [String] -> Expr

data Term where
  LInt :: Int -> Term
  LBool :: Bool -> Term
  LString :: String -> Term

deriving instance Show Expr
deriving instance Eq Expr
deriving instance Show Term
deriving instance Eq Term




builtInTypes :: [Type]
builtInTypes = [TPrimitive TInt, TPrimitive TBool, TPrimitive TString]
