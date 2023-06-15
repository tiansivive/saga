{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module Saga.AST.TypeSystem.Types where


import           Saga.AST.Syntax


data TypeExpr a where
  Type            :: Type a -> TypeExpr a
  TParens         :: a -> TypeExpr a -> TypeExpr a
  TConditional    :: a -> TypeExpr a -> TypeExpr a -> TypeExpr a -> TypeExpr a
  TClause         :: a -> [TypeExpr a] -> TypeExpr a -> TypeExpr a
  TBlock          :: a -> [TypeExpr a] -> TypeExpr a
  TReturn         :: a -> TypeExpr a -> TypeExpr a
  TLambda         :: a -> [Name a] -> TypeExpr a -> TypeExpr a
  TFnApp          :: a -> TypeExpr a -> [TypeExpr a] -> TypeExpr a



data Type a where
  TLiteral :: Term a -> Type a
  TPrimitive :: a -> BuiltInType -> Type a
  TTuple :: a -> [TypeExpr a] -> Type a
  TRecord :: a -> [(Name a, TypeExpr a)] -> Type a
  TArrow  :: a -> TypeExpr a -> TypeExpr a -> Type a

  TParametric  :: Name a -> TypeExpr a -> Type a

  TVar :: Name a -> Type a
  TIdentifier :: Name a -> Type a

  TConstrained    :: [PolymorphicVar a] -> [Constraint a] -> TypeExpr a -> Type a
  TProtocol       :: Name a -> Type a -> Type a
  TImplementation :: Name a -> TypeExpr a -> [RequiredImplId a] -> Type a

  TUnit :: Type a

type RequiredImplId = Name



data BuiltInType
    = TBool
    | TInt
    | TString
    deriving (Show, Eq)


data PolymorphicVar a = TPolyVar Quality Quantity (Name a) deriving (Foldable, Show, Eq)

data Constraint a
  = Implements (TypeExpr a) (TypeExpr a)
  | Extends (TypeExpr a) (TypeExpr a)
    deriving (Foldable, Show, Eq)

data Quality = Forall | Exists
  deriving (Eq, Show)
data Quantity = Linear | Affine | Relevant | None
  deriving (Eq, Show)


deriving instance Show a => Show (Type a)
deriving instance Show a => Show (TypeExpr a)

deriving instance Eq a => Eq (TypeExpr a)
deriving instance Eq a => Eq (Type a)

deriving instance Foldable TypeExpr
deriving instance Foldable Type
