{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module Saga.Language.TypeSystem.Types where

import           Data.Map                    (Map)
import           Data.Set                    (Set)
import           GHC.Base                    ()
import           Saga.Language.Core.Literals (Literal)
import           Saga.Lexer.Tokens           (Token (Qualified))

data TypeExpr where
  TAtom             :: Type -> TypeExpr
  TComposite        :: CompositeExpr -> TypeExpr
  TIdentifier       :: String -> TypeExpr
  TConditional      :: TypeExpr -> TypeExpr -> TypeExpr -> TypeExpr
  TClause           :: TypeExpr -> [Binding TypeExpr] -> TypeExpr
  TTagged           :: String -> TypeExpr -> TypeExpr

  TLambda           :: [String] -> TypeExpr -> TypeExpr
  TFnApp            :: TypeExpr -> [TypeExpr] -> TypeExpr

  TImplementation   :: ProtocolId -> TypeExpr -> TypeExpr
  TQualified        :: Qualified TypeExpr -> TypeExpr


data CompositeExpr where
  TEUnion   :: [TypeExpr] -> CompositeExpr
  TETuple   :: [TypeExpr] -> CompositeExpr
  TERecord  :: [(String, TypeExpr)] -> CompositeExpr
  TEArrow   :: TypeExpr -> TypeExpr -> CompositeExpr

data Binding a
  = Bind String a
  | ImplBind TypeExpr ProtocolId
  | SubtypeBind String a
  | RefineBind String a
  deriving (Show, Eq, Ord)

data Qualified t = (:=>) { constraints :: [Constraint], item:: t } --, mode :: Mode, multiplicity :: Multiplicity }
  deriving (Show, Eq, Ord)
infixr 0 :=>


data Constraint
  = Type `Implements` String
  -- | Extends Type Type
  deriving (Show, Eq, Ord)

type ProtocolId = String

data Type where
  TLiteral :: Literal -> Type
  TPrimitive :: PrimitiveType -> Type
  TTuple :: [Type] -> Type
  TRecord :: [(String, Type)] -> Type
  TUnion :: Set Type -> Type
  TArrow :: Type -> Type -> Type
  TData :: Tycon -> Type
  TClosure :: [String] -> TypeExpr -> Map String TypeExpr -> Type
  TApplied :: Type -> Type -> Type
  TVar :: Tyvar -> Type
  TVoid :: Type

data Tycon = Tycon String Kind deriving (Show, Eq, Ord)
data Tyvar = Tyvar String Kind deriving (Show, Eq, Ord)
-- instance Show Tyvar where
--   show (Tyvar s _) = s
-- instance Show Tycon where
--   show (Tycon s _) = s
data PrimitiveType
  = TBool
  | TInt
  | TString
  deriving (Show, Eq, Ord)

infixr 5 `TArrow`

deriving instance Show TypeExpr
deriving instance Eq TypeExpr
deriving instance Ord TypeExpr
deriving instance Show CompositeExpr
deriving instance Eq CompositeExpr
deriving instance Ord CompositeExpr
deriving instance Show Type
deriving instance Eq Type
deriving instance Ord Type

data Kind
  = KType
  | KArrow Kind Kind
  | KProtocol Kind
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




