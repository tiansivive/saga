{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}


module Saga.Language.Syntax.Literals where

import           Data.Data

data Literal where
  LInt :: Int -> Literal
  LBool :: Bool -> Literal
  LString :: String -> Literal

deriving instance Show Literal
deriving instance Eq Literal
deriving instance Ord Literal
deriving instance Data Literal
