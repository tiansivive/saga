{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module Saga.AST.TypeSystem.Kinds where

import Saga.AST.TypeSystem.Types
import Saga.AST.Syntax


type Argument = String
data Protocol a = Protocol (Name a) [Argument] [(String, Type a)]
  deriving (Show, Eq)



data Kind a
  = Value
  | Constructor (Kind a) (Kind a)
  | Implements (Protocol a)
  | Extends (Type a)
  | KVar String

deriving instance Show a => Show (Kind a)
deriving instance Eq a => Eq (Kind a)
