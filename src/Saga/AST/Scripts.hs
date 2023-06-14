{-# LANGUAGE DeriveFoldable #-}

module Saga.AST.Scripts where

import           Saga.AST.Syntax
import           Saga.AST.TypeSystem.Kinds
import           Saga.AST.TypeSystem.Types (TypeExpr)

data Script a =
    Script a (Module a) [Declaration a] [Import a]
        deriving (Show)

data Declaration a
  = Let  (Name a) (Maybe (TypeExpr a)) (Maybe (Kind a)) (Expr a)
  | Data (Name a) (Maybe (Kind a))     [(Name a, TypeExpr a)]
  | Type (Name a) (Maybe (Kind a))     (TypeExpr a)
    deriving (Foldable, Show, Eq)

data Import a = Import a [String]
  deriving (Foldable, Show)
data Module a = Mod a [String]
  deriving (Foldable, Show)
