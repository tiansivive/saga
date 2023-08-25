{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module Saga.AST.TypeSystem.HindleyMilner.Types where

import           Data.Map          (Map)
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
  TClause      :: TypeExpr -> [Binding TypeExpr] -> TypeExpr
  -- TBlock          :: [TypeExpr] -> TypeExpr
  -- TReturn         :: TypeExpr -> TypeExpr
  TEUnion        :: [TypeExpr] -> TypeExpr
  TTagged       :: String -> TypeExpr -> TypeExpr
  TLambda :: [String] -> TypeExpr -> TypeExpr
  TFnApp :: TypeExpr -> [TypeExpr] -> TypeExpr
  TImplementation :: ProtocolId -> TypeExpr -> TypeExpr
  TQuantified :: Quantifier -> TypeExpr -> TypeExpr
  TConstraint :: [Constraint] -> TypeExpr -> TypeExpr


data Binding a
  = Bind String a
  | ImplBind String String
  | SubtypeBind String a
  | RefineBind String a
  deriving (Show, Eq)



data Type where
  TLiteral :: Term -> Type
  TPrimitive :: BuiltInType -> Type
  TTuple :: [Type] -> Type
  TRecord :: [(String, Type)] -> Type
  TUnion :: [Type] -> Type
  TArrow :: Type -> Type -> Type
  TData :: Tycon -> Type
  TClosure :: [String] -> TypeExpr -> Map String Type -> Type
  TApplied :: Type -> Type -> Type
  TVar :: Tyvar -> Type
  -- TConstrained :: [Constraint] -> Type -> Type
  -- TProtocol         :: TypeExpr -> Type
  -- TImplementation   :: TypeExpr -> [RequiredImplId] -> Type
  TQualified :: Qualified Type -> Type
  TUnit :: Type

type ProtocolId = String
data Tycon = Tycon String Kind deriving ( Eq)
data Tyvar = Tyvar String Kind deriving ( Eq, Ord)
instance Show Tyvar where
  show (Tyvar s _) = s
instance Show Tycon where
  show (Tycon s _) = s
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
  | KArrow Kind Kind
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
  IfElse :: Expr -> Expr -> Expr -> Expr
  Match :: Expr -> [Case] -> Expr
  Lambda :: [String] -> Expr -> Expr
  FnApp :: Expr -> [Expr] -> Expr
  Clause :: Expr -> [Binding Expr] -> Expr

  Block :: [Expr] -> Expr
  Return :: Expr -> Expr
  Parens :: Expr -> Expr
  --FieldAccess :: Expr -> String -> Expr

data Term where
  LInt :: Int -> Term
  LBool :: Bool -> Term
  LString :: String -> Term


data Case = Case Pattern Expr deriving (Show, Eq)
data Pattern
    = Id String
    | Literal Term
    | PatTuple [String]
    | PatList [String] (Maybe String)
    | PatRecord [String] (Maybe String)
    | PatData String [String]
  deriving (Show, Eq)

deriving instance Show Expr
deriving instance Eq Expr
deriving instance Show Term
deriving instance Eq Term


type DataExpr = (String, TypeExpr)

data Declaration
    = Let String (Maybe TypeExpr) (Maybe Kind) Expr
    | Type String (Maybe Kind) TypeExpr
    | Data String (Maybe Kind) [DataExpr] [Binding TypeExpr]
    deriving (Show, Eq)
newtype Script = Script [Declaration]
    deriving (Show, Eq)

