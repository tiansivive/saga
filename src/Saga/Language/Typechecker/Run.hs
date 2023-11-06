module Saga.Language.Typechecker.Run where
import           Saga.Language.Core.Expr                                 (Expr)
import           Saga.Language.Typechecker.Inference.Inference           (Inference (infer))
import           Saga.Language.Typechecker.Inference.Type.Expr
import           Saga.Language.Typechecker.Inference.Type.Generalization
import           Saga.Language.Typechecker.Inference.Type.Instantiation

import qualified Effectful                                               as Eff
import qualified Effectful.Writer.Static.Local                           as Eff
import qualified Saga.Language.Typechecker.Solver.Constraints            as CST



run (expr :: Expr) = do
    (e, cs) <- Eff.runWriter @[CST.Constraint] $ Eff.inject $ infer expr
    let c = foldr CST.Conjunction CST.Empty cs

    _l
