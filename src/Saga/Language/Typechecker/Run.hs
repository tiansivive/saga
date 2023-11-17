{-# LANGUAGE DataKinds #-}
module Saga.Language.Typechecker.Run where
import           Saga.Language.Core.Expr                                 (Expr)
import           Saga.Language.Typechecker.Inference.Inference           (Inference (infer))
import           Saga.Language.Typechecker.Inference.Type.Expr
import           Saga.Language.Typechecker.Inference.Type.Generalization
import           Saga.Language.Typechecker.Inference.Type.Instantiation

import qualified Effectful                                               as Eff
import qualified Effectful.Error.Static                                  as Eff
import qualified Effectful.Fail                                          as Eff
import qualified Effectful.Reader.Static                                 as Eff
import qualified Effectful.State.Static.Local                            as Eff
import qualified Effectful.Writer.Static.Local                           as Eff
import           Saga.Language.Typechecker.Environment                   (Info)
import           Saga.Language.Typechecker.Errors                        (SagaError)
import qualified Saga.Language.Typechecker.Inference.Inference           as I
import qualified Saga.Language.Typechecker.Inference.Run                 as Inf
import           Saga.Language.Typechecker.Lib                           (defaultEnv)
import           Saga.Language.Typechecker.Monad                         (TypeCheck)
import qualified Saga.Language.Typechecker.Solver.Constraints            as CST
import qualified Saga.Language.Typechecker.Solver.Run                    as Solver
import           Saga.Language.Typechecker.Type                          (Polymorphic,
                                                                          Type)
import qualified Saga.Language.Typechecker.Zonking.Run                   as Zonking



typecheck :: Expr -> TypeCheck '[] (Expr, Polymorphic Type)
typecheck (expr :: Expr) = do
    ((ast, st), constraint) <- Inf.run expr
    context <- Eff.inject $ Solver.run constraint
    (ast', ty) <- Zonking.run ast context

    return (ast', ty)


run :: TypeCheck '[] a -> Either String (Either (Eff.CallStack, SagaError) (a, Info))
run = Eff.runPureEff . Eff.runFail . Eff.runError . Eff.runWriter . Eff.runReader defaultEnv
