{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFoldable #-}

module Saga.AST.Syntax where

import Data.ByteString.Lazy.Char8 (ByteString)


data Expr a where
  Assign :: Assignment a -> Expr a
  Lit    :: Literal a -> Expr a 
  Lambda :: a -> [Name a] -> Expr a -> Expr a

data Literal a where
  LInt    :: a -> Int -> Literal a
  LBool   :: a -> Bool -> Literal a
  LString :: a -> ByteString -> Literal a
  LArray  :: a -> [Expr a] -> Literal a
  LTuple  :: a -> [Expr a] -> Literal a
  LRecord :: a -> [(Name a, Expr a)] -> Literal a


data Assignment a = Assignment a (Name a) (Expr a)
    deriving (Foldable, Show)
data Name a = Name a ByteString
    deriving (Foldable, Show)


deriving instance Foldable Expr
deriving instance Foldable Literal

deriving instance Show a => Show (Expr a)
deriving instance Show a => Show (Literal a)


