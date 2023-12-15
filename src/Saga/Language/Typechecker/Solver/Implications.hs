module Saga.Language.Typechecker.Solver.Implications where
import           Effectful                                    (Eff)
import qualified Saga.Language.Typechecker.Solver.Constraints as C

import           Saga.Language.Typechecker.Solver.Constraints (Assumption,
                                                               Constraint)
import           Saga.Language.Typechecker.Solver.Monad
import           Saga.Language.Typechecker.Type               (Type)
import           Saga.Language.Typechecker.Variables          (Variable)


data Implies = Implies [Variable Type] [Assumption] Constraint

instance Solve Implies where
    solve = solve'
    simplify = simplify'


solve' :: SolverEff es => p -> Eff es (Status, Constraint)
solve' _ = return (Solved, C.Empty)
simplify' :: SolverEff es => Implies -> Eff es Constraint
simplify' (Implies vs as c) = return $ C.Implication vs as c
