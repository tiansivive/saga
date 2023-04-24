{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module Saga.AST.TypeSystem.Kinds where



data Kind a
  = Value
  | Constructor (Kind a) (Kind a)
  | Constraint
  | KVar String

deriving instance Show a => Show (Kind a)
deriving instance Eq a => Eq (Kind a)
