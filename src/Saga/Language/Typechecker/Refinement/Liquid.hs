{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}




module Saga.Language.Typechecker.Refinement.Liquid  where

import           Saga.Language.Core.Literals
import           Saga.Language.Typechecker.Variables (PolymorphicVar,
                                                      Restricted)

type instance Restricted String = ()

data Liquid where
    Var          :: String -> Liquid
    Number       :: Int -> Liquid
    Arithmetic   :: Op -> Liquid -> Liquid -> Liquid
    Comparison   :: Op -> Liquid -> Liquid -> Liquid
    Boolean      :: Bool -> Liquid
    Logical      :: Op -> Liquid -> Liquid -> Liquid
    Equality     :: Liquid -> Liquid -> Liquid
    Negation     :: Liquid -> Liquid


data Op = EQ | AND | OR | GT | GTE | LT | LTE | ADD | SUB | MUL | DIV | MOD | CONCAT
    deriving (Show, Eq, Ord)



deriving instance Show Liquid
deriving instance Ord Liquid
deriving instance Eq Liquid

