{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module Saga.Language.TypeSystem.HindleyMilner.Types where

import           Data.Map                    (Map)
import           GHC.Base                    ()
import           Saga.Language.Core.Literals (Literal)
import           Saga.Lexer.Tokens           (Token (Qualified))

data TypeExpr where
  TELiteral :: Literal -> TypeExpr
  TIdentifier :: String -> TypeExpr
  TETuple :: [TypeExpr] -> TypeExpr
  TERecord :: [(String, TypeExpr)] -> TypeExpr
  TEArrow :: TypeExpr -> TypeExpr -> TypeExpr
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
  TLiteral :: Literal -> Type
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




