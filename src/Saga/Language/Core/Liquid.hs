{-# LANGUAGE GADTs #-}

module Saga.Language.Core.Liquid where

import           Saga.Language.Core.Literals


data Expr where
    Literal         :: Literal -> Expr
    Identifier      :: String -> Expr
    Application     :: Expr -> [Expr] -> Expr


deriving instance Show Expr
deriving instance Ord Expr
deriving instance Eq Expr

