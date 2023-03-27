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
  Declaration   :: Definition a -> Expr a
  Lit           :: Literal a -> Expr a
  Flow          :: a -> Expr a -> Expr a -> Expr a -> Expr a
  Lambda        :: a -> [Name a] -> Expr a -> Expr a
  FnApp         :: a -> Expr a -> [Expr a] -> Expr a
  Block         :: a -> [Definition a] -> Expr a -> Expr a
  Identifier    :: Name a -> Expr a
  Parens        :: Expr a -> Expr a

data Literal a where
  LInt    :: a -> Int -> Literal a
  LBool   :: a -> Bool -> Literal a
  LString :: a -> ByteString -> Literal a
  LList   :: a -> [Expr a] -> Literal a
  LTuple  :: a -> [Expr a] -> Literal a
  LRecord :: a -> [(Name a, Expr a)] -> Literal a



data Definition a = Def a (Name a) (Expr a)
    deriving (Foldable, Show, Eq)

data Name a = Name a ByteString
    deriving (Foldable, Show, Eq)

data Import a = Import a Module
  deriving (Foldable, Show)
data ModuleDef a = DefMod a Module
  deriving (Foldable, Show)

deriving instance Foldable Expr
deriving instance Foldable Literal

deriving instance Show a => Show (Expr a)
deriving instance Show a => Show (Literal a)

deriving instance Eq a => Eq (Expr a)
deriving instance Eq a => Eq (Literal a)


newtype Module = Mod [ByteString]
    deriving (Show)


