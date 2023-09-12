{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module Saga.Parser.Literals where

data Literal where
  LInt :: Int -> Literal
  LBool :: Bool -> Literal
  LString :: String -> Literal

deriving instance Show Literal
deriving instance Eq Literal
