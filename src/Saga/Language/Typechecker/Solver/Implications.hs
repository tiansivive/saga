module Saga.Language.Typechecker.Solver.Implications where
import           Effectful                                    (Eff)
import           Saga.Language.Typechecker.Solver.Constraints
import           Saga.Language.Typechecker.Solver.Monad
import           Saga.Language.Typechecker.Type               (Type)
import           Saga.Language.Typechecker.Variables          (Variable)


data Implies = Implies [Variable Type] [Assumption] Constraint

instance Solve Implies where
    solve = solve'
    simplify = simplify'


solve' :: SolverEff es => p -> Eff es (Status, Constraint)
solve' _ = return (Solved, Empty)
simplify' :: SolverEff es => p -> Eff es Constraint
simplify' _ = return Empty
