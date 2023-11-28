module Saga.Language.Typechecker.Solver.Refinements where
import           Saga.Language.Typechecker.Refinement.Liquid  (Liquid)
import           Saga.Language.Typechecker.Solver.Constraints (Item, Scope)
import           Saga.Language.Typechecker.Solver.Entailment  (Entails (..))
import           Saga.Language.Typechecker.Solver.Monad       (Solve (..))





data Refinement = Refine Scope Item Liquid



instance Entails Refinement where
    -- | FIXME: Implement refinement entailment by generating implication constraints
    entails r = return



instance Solve Refinement where
    solve = _solve'
    simplify = simplify'




simplify' r = do

    _r
