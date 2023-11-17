module Saga.Language.Typechecker.Solver.Implications where
import           Saga.Language.Typechecker.Solver.Constraints
import           Saga.Language.Typechecker.Solver.Monad
import           Saga.Language.Typechecker.Type               (Type)
import           Saga.Language.Typechecker.Variables          (PolymorphicVar)


data Implies = Implies [PolymorphicVar Type] [Assumption] Constraint

instance Solve Implies where
    solve = solve'
    simplify = simplify'


solve' _ = return (Solved, Empty)
simplify' _ = return Empty
