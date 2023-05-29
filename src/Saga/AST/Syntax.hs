{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module Saga.AST.Syntax where

import           Data.ByteString.Lazy.Char8 (ByteString)




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

data Name a = Name a String
    deriving (Foldable, Show, Eq)


deriving instance Foldable Expr
deriving instance Foldable Term

deriving instance Show a => Show (Expr a)
deriving instance Show a => Show (Term a)

deriving instance Eq a => Eq (Expr a)
deriving instance Eq a => Eq (Term a)
