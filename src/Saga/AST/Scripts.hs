{-# LANGUAGE DeriveFoldable     #-}

module Saga.AST.Scripts where

import Saga.AST.Syntax
import Saga.AST.TypeSystem.Types

data Script a =
    Script a (Module a) [Declaration a] [Import a]
        deriving (Show)

data Declaration a
  = Define (Name a) (Expr a) (Maybe (TypeExpr a))
  | Data (Name a) [TypeExpr a] [(Name a, [TypeExpr a])]
    deriving (Foldable, Show, Eq)

data Import a = Import a [String]
  deriving (Foldable, Show)
data Module a = Mod a [String]
  deriving (Foldable, Show)