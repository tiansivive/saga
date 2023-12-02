{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}




module Saga.Language.Typechecker.Refinement.Liquid  where

import           Saga.Language.Core.Literals
import           Saga.Language.Typechecker.Variables (PolymorphicVar)



data Liquid where
    Var          :: (PolymorphicVar Liquid) -> Liquid
    Number       :: Int -> Liquid
    Arithmetic   :: Op -> Liquid -> Liquid -> Liquid
    Comparison   :: Op -> Liquid -> Liquid -> Liquid
    Boolean      :: Bool -> Liquid
    Logical      :: Op -> Liquid -> Liquid -> Liquid
    Equality     :: Liquid -> Liquid -> Liquid
    Negation     :: Liquid -> Liquid

deriving instance Show Liquid
deriving instance Ord Liquid
deriving instance Eq Liquid


data instance PolymorphicVar Liquid where
    Liquid :: String -> PolymorphicVar Liquid
deriving instance Show (PolymorphicVar Liquid)
deriving instance Eq (PolymorphicVar Liquid)
deriving instance Ord (PolymorphicVar Liquid)



data Op = EQ | AND | OR | GT | GTE | LT | LTE | ADD | SUB | MUL | DIV | MOD | CONCAT
    deriving (Show, Eq, Ord)



