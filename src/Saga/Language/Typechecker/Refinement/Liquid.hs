{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}




module Saga.Language.Typechecker.Refinement.Liquid  where

import           Saga.Language.Core.Literals
import           Saga.Language.Typechecker.Variables (Variable)



data Liquid where
    Var          :: (Variable Liquid) -> Liquid
    Number       :: Int -> Liquid
    Boolean      :: Bool -> Liquid
    Logical      :: Op -> Liquid -> Liquid -> Liquid
    Arithmetic   :: Op -> Liquid -> Liquid -> Liquid
    Comparison   :: Op -> Liquid -> Liquid -> Liquid
    Equality     :: Liquid -> Liquid -> Liquid
    Negation     :: Liquid -> Liquid

deriving instance Show Liquid
deriving instance Ord Liquid
deriving instance Eq Liquid


data instance Variable Liquid where
    Poly :: String -> Variable Liquid
deriving instance Show (Variable Liquid)
deriving instance Eq (Variable Liquid)
deriving instance Ord (Variable Liquid)



data Op = EQ | AND | OR | GT | GTE | LT | LTE | ADD | SUB | MUL | DIV | MOD | CONCAT
    deriving (Show, Eq, Ord)



