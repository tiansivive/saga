{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Saga.Language.Typechecker.Elaboration.Monad where

import qualified Data.Kind                                     as GHC

import           Effectful                                     (Eff, (:>))
import qualified Effectful                                     as Eff
import qualified Effectful.State.Static.Local                  as Eff

import qualified Effectful.Error.Static                        as Eff
import qualified Effectful.Fail                                as Eff
import qualified Effectful.Reader.Static                       as Eff
import qualified Effectful.Writer.Static.Local                 as Eff
import           Saga.Language.Syntax.AST
import           Saga.Language.Syntax.Polymorphism             (Polymorphic (..))
import           Saga.Language.Typechecker.Env                 (CompilerState,
                                                                Info)
import           Saga.Language.Typechecker.Errors              (SagaError)
import qualified Saga.Language.Typechecker.Solving.Constraints as Solver
import qualified Saga.Language.Typechecker.Variables           as Var
import           Saga.Utils.Operators                          ((|>))




class Elaboration (e :: NodeType) where
    type family Effects e (es :: [Eff.Effect]) :: GHC.Constraint
    elaborate :: Effects e es => AST Reduced e -> Eff es (AST Elaborated e)

class Instantiate t where
    instantiate :: Polymorphic t -> t -> t

-- QUESTION: Is this the right place to define Generalization? It should now happen only after zonking, so there's no need for Inference to depend on it.
class Generalize t where
    -- ENHANCEMENT: Define the needed effects as an associated type
    generalize :: (Eff.State Int :> es)  => t -> Eff es (Polymorphic t)



run initialEnv level initialState = elaborate
        |> Eff.runWriter @Solver.Constraint
        |> Eff.runState initialState
        |> Eff.runReader (Var.Level level)
        |> Eff.runReader @(CompilerState Elaborated) initialEnv
        |> Eff.runWriter @Info
        |> Eff.runError @SagaError
        |> Eff.runFail
        |> Eff.runEff

