{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}



module Saga.Language.Typechecker.Refinement.Liquid  where

import           Saga.Language.Core.Literals
import           Saga.Language.Typechecker.Variables (PolymorphicVar,
                                                      Restricted)

type instance Restricted String = ()

data Liquid where
    Literal         :: Literal -> Liquid
    Identifier      :: String -> Liquid
    Var             :: PolymorphicVar String -> Liquid
    BinaryOp        :: Operator -> Liquid -> Liquid -> Liquid
    Negation        :: Liquid -> Liquid

data Operator = AND | OR | EQ | GT | GTE | LT | LTE deriving (Show, Eq, Ord)


deriving instance Show Liquid
deriving instance Ord Liquid
deriving instance Eq Liquid

