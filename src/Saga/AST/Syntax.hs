{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module Saga.AST.Syntax where

import           Data.ByteString.Lazy.Char8 (ByteString)



data Script a =
    Script a (Module a) [Declaration a] [Import a]
        deriving (Show)

data Expr a where
  Term           :: Term a -> Expr a
  Identifier     :: Name a -> Expr a
  Assign         :: Name a -> Expr a -> Expr a
  IfElse         :: a -> Expr a -> Expr a -> Expr a -> Expr a
  Lambda         :: a -> [Name a] -> Expr a -> Expr a
  FnApp          :: a -> Expr a -> [Expr a] -> Expr a
  Clause         :: a -> [Expr a] -> Expr a -> Expr a
  Block          :: a -> [Expr a] -> Expr a
  Return         :: a -> Expr a -> Expr a
  Parens         :: a -> Expr a -> Expr a
  FieldAccess    :: a -> Expr a -> [Name a] -> Expr a

-- | AKA Literal term
data Term a where
  LInt    :: a -> Int -> Term a
  LBool   :: a -> Bool -> Term a
  LString :: a -> ByteString -> Term a
  LList   :: a -> [Expr a] -> Term a
  LTuple  :: a -> [Expr a] -> Term a
  LRecord :: a -> [(Name a, Expr a)] -> Term a



data TypeExpr a where
  Type            :: Type a -> TypeExpr a
  TParens         :: a -> TypeExpr a -> TypeExpr a
  TConditional    :: a -> TypeExpr a -> TypeExpr a -> TypeExpr a -> TypeExpr a
  TClause         :: a -> [Declaration a] -> TypeExpr a -> TypeExpr a
  TBlock          :: a -> [TypeExpr a] -> TypeExpr a
  TReturn         :: a -> TypeExpr a -> TypeExpr a
  TLambda         :: a -> [Name a] -> TypeExpr a -> TypeExpr a
  TFnApp          :: a -> TypeExpr a -> [TypeExpr a] -> TypeExpr a

-- | AKA Literal type
data Type a where
  TLiteral :: Term a -> Type a
  TTuple :: a -> [TypeExpr a] -> Type a
  TRecord :: a -> [(Name a, TypeExpr a)] -> Type a
  TArrow  :: a -> TypeExpr a -> TypeExpr a -> Type a
  TIdentifier    :: Name a -> Type a
  TPrimitive    :: a -> BuiltInType -> Type a
  TParametric  :: TypeExpr a -> TypeExpr a -> Type a
  TPolymorphic :: Name a -> Type a
  TVoid :: Type a

data BuiltInType
    = TBool
    | TInt
    | TString
    deriving (Show, Eq)

data Declaration a
  = Define a (Name a) (Expr a) (Maybe (TypeExpr a))
  | Data a (Name a) [(String, [TypeExpr a])]
    deriving (Foldable, Show, Eq)

data Name a = Name a String
    deriving (Foldable, Show, Eq)

data Import a = Import a [String]
  deriving (Foldable, Show)
data Module a = Mod a [String]
  deriving (Foldable, Show)

deriving instance Foldable Expr
deriving instance Foldable Term
deriving instance Foldable TypeExpr
deriving instance Foldable Type

deriving instance Show a => Show (Expr a)
deriving instance Show a => Show (Term a)
deriving instance Show a => Show (TypeExpr a)
deriving instance Show a => Show (Type a)

deriving instance Eq a => Eq (Expr a)
deriving instance Eq a => Eq (Term a)
deriving instance Eq a => Eq (TypeExpr a)
deriving instance Eq a => Eq (Type a)


