{-# LANGUAGE TypeFamilies #-}
module Saga.Language.Typechecker.Solver.Entailment where
import           Control.Monad                                (forM)
import           Data.Functor                                 ((<&>))
import qualified Data.List                                    as List
import qualified Data.Map                                     as Map
import           Data.Maybe                                   (catMaybes,
                                                               isJust)
import           Debug.Pretty.Simple                          (pTraceM)
import           Effectful                                    (Eff)
import qualified Effectful.Error.Static                       as Eff
import qualified Effectful.State.Static.Local                 as Eff
import           Saga.Language.Typechecker.Errors             (SagaError (..))
import qualified Saga.Language.Typechecker.Solver.Constraints as C
import           Saga.Language.Typechecker.Solver.Constraints (Constraint)
import           Saga.Language.Typechecker.Solver.Monad       (Solution (..),
                                                               SolverEff)
import           Saga.Utils.Operators                         ((|>), (||>))



-- | ISSUE #23
-- | TODO #24 Move Constraint list to a state effect within the Solver monad
class Entails a where
    entails :: SolverEff es => a -> [Constraint] -> Eff es [Constraint]



