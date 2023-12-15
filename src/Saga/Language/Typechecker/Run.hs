{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
module Saga.Language.Typechecker.Run where
import qualified Saga.Language.Core.Expr                                 as E
import           Saga.Language.Core.Expr                                 (Expr)
import           Saga.Language.Typechecker.Inference.Inference           (Inference (infer))

import           Saga.Language.Typechecker.Inference.Type.Generalization
import           Saga.Language.Typechecker.Inference.Type.Instantiation

import           Debug.Pretty.Simple                                     (pTraceM)
import           Effectful                                               (Eff)
import qualified Effectful                                               as Eff
import qualified Effectful.Error.Static                                  as Eff
import qualified Effectful.Fail                                          as Eff
import qualified Effectful.Reader.Static                                 as Eff
import qualified Effectful.State.Static.Local                            as Eff
import qualified Effectful.Writer.Static.Local                           as Eff
import           Saga.Language.Core.Literals                             (Literal (..))
import           Saga.Language.Typechecker.Environment                   (CompilerState,
                                                                          Info)
import           Saga.Language.Typechecker.Errors                        (SagaError)
import qualified Saga.Language.Typechecker.Inference.Inference           as I
import qualified Saga.Language.Typechecker.Inference.Type.Expr           as TI
import qualified Saga.Language.Typechecker.Inference.Type.Shared         as TI

import           Saga.Language.Typechecker.Lib                           (defaultEnv)
import           Saga.Language.Typechecker.Monad                         (TypeCheck)
import qualified Saga.Language.Typechecker.Solver.Constraints            as CST
import           Saga.Language.Typechecker.Solver.Cycles                 (Cycle)
import           Saga.Language.Typechecker.Solver.Monad                  (Count (..),
                                                                          Solve (..),
                                                                          initialCount,
                                                                          initialSolution)
import qualified Saga.Language.Typechecker.Solver.Run                    as Solver
import           Saga.Language.Typechecker.Type                          (Polymorphic,
                                                                          Type)
import qualified Saga.Language.Typechecker.Variables                     as Var
import qualified Saga.Language.Typechecker.Zonking.Run                   as Zonking
import           Saga.Utils.Operators                                    ((|>))
import           Saga.Utils.TypeLevel                                    (type (§))



typecheck :: TypeCheck es => Expr -> Eff es (Expr, Polymorphic Type)
typecheck expr  = do
    ((ast, st), constraint) <- Eff.runWriter @CST.Constraint . Eff.runState TI.initialState . Eff.runReader (Var.Level 0) $ infer expr
    pTraceM $ "AST:\n" ++ show ast
    pTraceM $ "State:\n" ++ show st
    context <- Eff.inject $ Solver.run constraint
    (ast', ty) <- Zonking.run ast context

    return (ast', ty)



run :: Eff '[Eff.Reader CompilerState, Eff.Writer Info, Eff.Error SagaError, Eff.Fail, Eff.IOE] a -> IO § Either String (Either (Eff.CallStack, SagaError) (a, Info))
run = Eff.runReader defaultEnv |> Eff.runWriter |> Eff.runError |> Eff.runFail |> Eff.runEff



test e = run $ do
    ((e, st), cs) <- TI.run e
    Eff.runState initialSolution . Eff.evalState initialCount .  Eff.runState @[Cycle Type] [] . Eff.runReader (Var.Level 0) $ simplified cs
        where
            simplified = mapM simplify . Solver.flatten

int = E.Literal . LInt

str = E.Literal . LString


gt x y = E.FnApp (E.Identifier ">") [x, y]
gte x y = E.FnApp (E.Identifier ">=") [x, y]
lt x y = E.FnApp (E.Identifier "<") [x, y]
lte x y = E.FnApp (E.Identifier "<=") [x, y]


foo = E.FnApp (E.Identifier "/") [E.Literal $ LInt 1, E.Literal $ LInt 0]

