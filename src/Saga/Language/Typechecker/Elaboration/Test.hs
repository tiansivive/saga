module Saga.Language.Typechecker.Elaboration.Test where

import qualified Data.Map                                                 as Map
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
import qualified Saga.Language.Syntax.Reduced.AST                         as RD
import           Saga.Language.Syntax.Reduced.Types                       (Type)
import qualified Saga.Language.Syntax.Reduced.Values                      as RD
import qualified Saga.Language.Typechecker.Elaboration.Effects            as Effs
import           Saga.Language.Typechecker.Elaboration.Effects            (State (..))
import           Saga.Language.Typechecker.Elaboration.Monad              (Elaboration (..))
import           Saga.Language.Typechecker.Env                            (CompilerState (..),
                                                                           Info,
                                                                           Proofs (..))
import qualified Saga.Language.Typechecker.Solving.Constraints            as Solver
import qualified Saga.Language.Typechecker.Variables                      as Var
import           Saga.Utils.Operators                                     ((|>),
                                                                           (||>))

import           Prelude                                                  hiding
                                                                          (print)
import           Saga.Language.Syntax.Literals                            (Literal (..))
import           Saga.Language.Typechecker.Elaboration.Values.Expressions
import           Saga.Language.Typechecker.Errors                         (SagaError)
import           Text.Pretty.Simple                                       (pPrint)



fn = RD.Raw $
    RD.Lambda ["x"] $
        RD.Raw (RD.Application
            (RD.Raw (RD.Var "x"))
            [RD.Raw (RD.Literal $ LInt 1)]
            )





run = Eff.runWriter @Solver.Constraint
        |> Eff.runState initialState
        |> Eff.runReader (Var.Level 0)
        |> Eff.runReader @(CompilerState Elaborated) initialEnv
        |> Eff.runWriter @Info
        |> Eff.runError @SagaError
        |> Eff.runFail
        |> Eff.runEff


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
    let (((ast, constraints), st), info ) = res
    putStrLn "\n---------------------\nINFO:\n"
    pPrint info
    putStrLn "\n---------------------\nSTATE:\n"
    pPrint st
    putStrLn "\n---------------------\nCONSTRAINTS:\n"
    pPrint constraints
    putStrLn "\n---------------------\nAST:\n"
    pPrint ast
    return ()


test = elaborate fn ||> run >>= print


initialEnv :: CompilerState Elaborated
initialEnv = Saga [] Map.empty Map.empty Map.empty (Proofs EL.Void Map.empty)

initialState :: State
initialState = IST 0 0 0 Map.empty
