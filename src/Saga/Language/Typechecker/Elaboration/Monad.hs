{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Typechecker.Elaboration.Monad where

import qualified Data.Kind                as GHC

import           Effectful                (Eff)
import qualified Effectful                as Eff

import           Saga.Language.Syntax.AST



class Elaboration (e :: NodeType) where
    type family Effects e (es :: [Eff.Effect]) :: GHC.Constraint
    elaborate :: Effects e es => AST Evaluated e -> Eff es (AST Elaborated e)

