{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Typechecker.Run where

import           Effectful                                                (Eff)
import qualified Effectful                                                as Eff
import qualified Effectful.Error.Static                                   as Eff
import qualified Effectful.Fail                                           as Eff
import qualified Effectful.Reader.Static                                  as Eff
import qualified Effectful.State.Static.Local                             as Eff
import qualified Effectful.Writer.Static.Local                            as Eff


import qualified Saga.Language.Syntax.AST                                 as NT (NodeType (..))
import           Saga.Language.Syntax.AST                                 (AST,
                                                                           Phase (..))

import qualified Saga.Language.Syntax.Elaborated.Types                    as EL

import           Saga.Language.Typechecker.Elaboration.Effects            (State (..))
import           Saga.Language.Typechecker.Elaboration.Monad              (Elaboration (..),
                                                                           elaborate)
import           Saga.Language.Typechecker.Elaboration.Values.Expressions

import           Saga.Language.Typechecker.Env                            (CompilerState (..),
                                                                           Info,
                                                                           Proofs (..))
import           Saga.Language.Typechecker.Errors                         (SagaError)
import qualified Saga.Language.Typechecker.Lib                            as Lib

import qualified Saga.Language.Typechecker.Solving.Constraints            as Solver
import           Saga.Language.Typechecker.Solving.Cycles                 (Cycle,
                                                                           collapse)
import qualified Saga.Language.Typechecker.Solving.Monad                  as Solver
import           Saga.Language.Typechecker.Solving.Monad                  (Count (..),
                                                                           Levels,
                                                                           Solution (..))
import qualified Saga.Language.Typechecker.Solving.Run                    as Solver

import qualified Saga.Language.Typechecker.Variables                      as Var

import           Saga.Language.Typechecker.Zonking.Monad                  (Context (..))
import qualified Saga.Language.Typechecker.Zonking.Run                    as Zonk



import           Control.Monad                                            (foldM)
import           Saga.Utils.Operators                                     ((|>))


typecheck e = run pipeline

    where
        pipeline = do
            ((ast, constraint), state) <- runElaboration $ elaborate e
            (((residuals, solution), count), cycles) <- runSolver $ Solver.process constraint
            tvars' <- foldM collapse (Solver.tvars solution) cycles
            let sol = solution { Solver.tvars = tvars' }
            (zonked, ty) <- runZonking sol residuals $ Zonk.run ast


            return (ast, constraint, state, residuals, sol, cycles, zonked, ty)



run =  Eff.runReader @(CompilerState Elaborated) initialEnv
        |> Eff.runWriter @Info
        |> Eff.runError @SagaError
        |> Eff.runFail
        |> Eff.runEff
        where
            initialEnv :: CompilerState Elaborated
            initialEnv = Saga [Lib.numProtocol] mempty mempty mempty (Proofs EL.Void mempty)


runElaboration = Eff.runWriter @Solver.Constraint
            |> Eff.runState initialState
            |> Eff.runReader (Var.Level 0)

        where
            initialState :: State
            initialState = IST 0 0 0 mempty



runSolver = Eff.runState initialSolution
        |> Eff.runState (Count 0 0 0)
        |> Eff.runState @[Cycle EL.Type] []
        |> Eff.runReader @Levels mempty
        |> Eff.runReader (Var.Level 0)

        where
            initialSolution :: Solution
            initialSolution = Solution mempty mempty mempty mempty mempty
            initialCount :: Count
            initialCount = Count 0 0 0

runZonking sol residuals = Eff.runReader (Context sol residuals)
