module Saga.Language.Typechecker.Test where


import           Saga.Language.Typechecker.Elaboration.Monad              (Elaboration (..),
                                                                           elaborate)
import qualified Saga.Language.Typechecker.Solving.Constraints            as Solver

import           Effectful                                                (Eff)
import qualified Effectful.Reader.Static                                  as Eff
import qualified Effectful.State.Static.Local                             as Eff
import qualified Effectful.Writer.Static.Local                            as Eff
import qualified Saga.Language.Syntax.AST                                 as NT (NodeType (..))
import           Saga.Language.Syntax.AST                                 (AST,
                                                                           Phase (..))
import qualified Saga.Language.Syntax.Elaborated.Types                    as EL
import qualified Saga.Language.Typechecker.Elaboration.Effects            as Effs hiding
                                                                                  (kvars,
                                                                                   tvars)
import           Saga.Language.Typechecker.Elaboration.Effects            (State (..))
import           Saga.Language.Typechecker.Env                            (CompilerState (..),
                                                                           Info,
                                                                           Proofs (..))
import qualified Saga.Language.Typechecker.Lib                            as Lib
import           Saga.Language.Typechecker.Solving.Cycles                 (Cycle)
import           Saga.Language.Typechecker.Solving.Monad                  (Count (..),
                                                                           Levels,
                                                                           Solution (..))
import qualified Saga.Language.Typechecker.Solving.Run                    as Solver
import qualified Saga.Language.Typechecker.Variables                      as Var
import           Saga.Language.Typechecker.Zonking.Monad                  (Context (..))
import qualified Saga.Language.Typechecker.Zonking.Run                    as Zonk
import           Saga.Utils.Operators                                     ((|>))

import qualified Effectful                                                as Eff
import qualified Effectful.Error.Static                                   as Eff
import qualified Effectful.Fail                                           as Eff
import           Prelude                                                  hiding
                                                                          (print)
import           Saga.Language.Syntax.Literals                            (Literal (..))
import qualified Saga.Language.Syntax.Reduced.AST                         as RD
import qualified Saga.Language.Syntax.Reduced.Values                      as RD
import           Saga.Language.Typechecker.Elaboration.Values.Expressions
import           Saga.Language.Typechecker.Errors                         (SagaError)
import           Text.Pretty.Simple                                       (pPrint)






fn = RD.Raw $
    RD.Lambda ["x"] $
        RD.Raw (RD.Application
            (RD.Raw (RD.Var "x"))
            [RD.Raw (RD.Literal $ LInt 1)]
            )

test = typecheck fn >>= print



typecheck e = run pipeline

    where
        pipeline = do
            ((ast, constraint), state) <- runElaboration $ elaborate e
            (((residuals, solution), count), cycles) <- runSolver $ Solver.process constraint
            (zonked, ty) <- runZonking solution residuals $ Zonk.run ast


            return (ast, constraint, state, residuals, solution, cycles, zonked, ty)



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



print (Left fail) = do
    putStrLn "FAILURE:"
    pPrint fail
    return ()
print (Right (Left (callstack, err))) = do
    putStrLn "\n---------------------\nSAGA ERROR:\n"
    pPrint err
    putStrLn "\n---------------------\nCallstack:\n"
    pPrint callstack
    return ()
print (Right (Right res)) = do
    let ((ast, constraint, state, residuals, solution, cycles, zonked, ty), info) = res
    putStrLn "\n--------------------- INFO --------------------- "
    pPrint info
    putStrLn "\n--------------------- AST --------------------- "
    pPrint ast
    putStrLn "\n--------------------- CONSTRAINT --------------------- "
    pPrint constraint
    putStrLn "\n--------------------- STATE --------------------- "
    pPrint state
    putStrLn "\n--------------------- RESIDUALS --------------------- "
    pPrint residuals
    putStrLn "\n--------------------- CYCLES --------------------- "
    pPrint cycles
    putStrLn "\n--------------------- SOLUTION --------------------- "
    pPrint solution
    putStrLn "\n--------------------- ZONKED --------------------- "
    pPrint zonked
    putStrLn "\n--------------------- QUALIFIED TYPE --------------------- "
    pPrint ty

    return ()

