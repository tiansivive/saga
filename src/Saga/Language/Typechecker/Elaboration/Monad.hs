{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Saga.Language.Typechecker.Elaboration.Monad where

import qualified Data.Kind                         as GHC

import           Effectful                         (Eff, (:>))
import qualified Effectful                         as Eff
import qualified Effectful.State.Static.Local      as Eff

import           Saga.Language.Syntax.AST
import           Saga.Language.Syntax.Polymorphism (Polymorphic (..))




class Elaboration (e :: NodeType) where
    type family Effects e (es :: [Eff.Effect]) :: GHC.Constraint
    elaborate :: Effects e es => AST Evaluated e -> Eff es (AST Elaborated e)

class Instantiate t where
    instantiate :: Polymorphic t -> t -> t

-- QUESTION: Is this the right place to define Generalization? It should now happen only after zonking, so there's no need for Inference to depend on it.
class Generalize t where
    -- ENHANCEMENT: Define the needed effects as an associated type
    generalize :: (Eff.State Int :> es)  => t -> Eff es (Polymorphic t)



