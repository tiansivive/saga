{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}


module Saga.Language.Core.Literals where

data Literal where
  LInt :: Float -> Literal
  LBool :: Bool -> Literal
  LString :: String -> Literal

deriving instance Show Literal
deriving instance Eq Literal
deriving instance Ord Literal
