
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module Saga.AST.TypeSystem.Kinds where



data Kind a
  = KValue
  | KConstructor (Kind a) (Kind a)
  | KConstraint
  | KProtocol
  | KVar String

deriving instance Show a => Show (Kind a)
deriving instance Eq a => Eq (Kind a)
deriving instance Foldable Kind
