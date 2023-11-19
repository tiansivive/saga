module Saga.Language.Typechecker.Inference.Run where
import qualified Effectful                                     as Eff
import qualified Effectful.State.Static.Local                  as Eff
import qualified Effectful.Writer.Static.Local                 as Eff
import           Saga.Language.Core.Expr                       (Expr)
import qualified Saga.Language.Typechecker.Inference.Inference as I
import           Saga.Language.Typechecker.Inference.Inference (Inference (..))
import           Saga.Language.Typechecker.Monad               (TypeCheck)
import qualified Saga.Language.Typechecker.Solver.Constraints  as C

import           Saga.Language.Typechecker.Inference.Type.Expr

run :: Expr -> TypeCheck es ((Expr, I.State), C.Constraint)
run = Eff.runWriter @C.Constraint . Eff.runState I.initialState . Eff.inject . infer
