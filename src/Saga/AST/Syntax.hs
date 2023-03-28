{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module Saga.AST.Syntax where

import           Data.ByteString.Lazy.Char8 (ByteString)



data Script a =
    Script a (ModuleDef a) [Definition a] [Import a]
        deriving (Show)

data Expr a where
  Declaration    :: Definition a -> Expr a
  Term           :: Term a -> Expr a
  Type           :: Type a -> Expr a
  Flow           :: a -> Expr a -> Expr a -> Expr a -> Expr a
  Lambda         :: a -> [Name a] -> Expr a -> Expr a
  FnApp          :: a -> Expr a -> [Expr a] -> Expr a
  Clause         :: a -> [Definition a] -> Expr a -> Expr a
  Block          :: a -> [Expr a] -> Expr a
  Return         :: a -> Expr a -> Expr a
  Identifier     :: Name a -> Expr a
  Parens         :: a -> Expr a -> Expr a
  TypeAnnotation :: Expr a -> Type a -> Expr a

-- | AKA Literal term
data Term a where
  LInt    :: a -> Int -> Term a
  LBool   :: a -> Bool -> Term a
  LString :: a -> ByteString -> Term a
  LList   :: a -> [Expr a] -> Term a
  LTuple  :: a -> [Expr a] -> Term a
  LRecord :: a -> [(Name a, Expr a)] -> Term a

-- | AKA Literal type
data Type a where
  TInt    :: a -> Type a
  TBool   :: a -> Type a
  TString :: a -> Type a
  TArrow  :: a -> [Type a] -> Type a -> Type a
  TVar    :: Name a -> Type a
  TParam  :: a -> Name a -> [Type a] -> Type a

data Definition a
  = Def a (Name a) (Expr a)
 -- | Data a (Name a) [(String, [Type a])]
    deriving (Foldable, Show, Eq)

data Name a = Name a String
    deriving (Foldable, Show, Eq)

data Import a = Import a Module
  deriving (Foldable, Show)
data ModuleDef a = DefMod a Module
  deriving (Foldable, Show)

deriving instance Foldable Expr
deriving instance Foldable Type
deriving instance Foldable Term

deriving instance Show a => Show (Expr a)
deriving instance Show a => Show (Type a)
deriving instance Show a => Show (Term a)

deriving instance Eq a => Eq (Expr a)
deriving instance Eq a => Eq (Type a)
deriving instance Eq a => Eq (Term a)


newtype Module = Mod [String]
    deriving (Show)


