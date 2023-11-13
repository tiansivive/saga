
module Saga.Language.Typechecker.Solver.Run where
import           Control.Arrow                                   (ArrowChoice (left))
import           Control.Monad                                   (foldM, forM)
import qualified Effectful                                       as Eff
import qualified Effectful.State.Static.Local                    as Eff
import qualified Effectful.Writer.Static.Local                   as Eff
import           Saga.Language.Core.Expr                         (Expr)
import           Saga.Language.Typechecker.Inference.Inference   (initialState)
import           Saga.Language.Typechecker.Inference.Type.Shared (TypeInference)
import qualified Saga.Language.Typechecker.Solver.Constraints    as C
import           Saga.Language.Typechecker.Solver.Constraints    (Constraint (Conjunction))
import qualified Saga.Language.Typechecker.Solver.Monad          as Solve
import           Saga.Language.Typechecker.Solver.Monad
import qualified Saga.Language.Typechecker.Solver.Protocols      as P

import           Saga.Language.Typechecker.Type                  (Type)

import           Prelude                                         hiding (Eq)

import qualified Data.List                                       as List
import qualified Data.Map                                        as Map
import qualified Effectful.Reader.Static                         as Eff
import qualified Saga.Language.Typechecker.Inference.Inference   as I
import           Saga.Language.Typechecker.Solver.Cycles         (Cycle,
                                                                  collapse)
import qualified Saga.Language.Typechecker.Solver.Equalities     as E
import           Saga.Language.Typechecker.Solver.Substitution   (Subst,
                                                                  Substitutable (..))
import           Saga.Utils.Operators                            ((|>), (||>))

import           Debug.Pretty.Simple                             (pTrace,
                                                                  pTraceM,
                                                                  pTraceShow)
import           Debug.Trace                                     (trace)
import           Saga.Language.Typechecker.Inference.Type.Expr
import           Saga.Language.Typechecker.Solver.Entailment     (entailment)


run :: TypeInference Expr -> SolverM ((String, Expr), (String, Constraint, String, [Constraint]), Subst Type, Subst C.Evidence, I.State, Solution)
run inference = do
    ((ast, inferenceState), constraint) <- Eff.runWriter @C.Constraint $ Eff.runState I.initialState $ Eff.inject inference

    ((residuals, cycles), solution) <- Eff.runState initialSolution . Eff.runWriter @[Cycle Type] . Eff.inject $ process (flatten constraint)
    types <- foldM collapse (tvars solution) cycles

    return (("AST", ast), ("Emitted", constraint, "Residuals", residuals), types, evidence solution, inferenceState, solution)

    where
        process :: [C.Constraint] -> SolverM [C.Constraint]
        process [] = return []
        process cs = do
            results <- step cs >>= mapM solve
            let next = fmap snd results
            if all deferred results then
                return next
            else process next

        deferred (Deferred, _) = True
        deferred _             = False


step :: [C.Constraint] -> SolverM [C.Constraint]
step cs = do
    Solution { evidence, tvars } <- Eff.get
    simplified <- forM cs simplify
    entailment $ simplified ||> apply tvars >>= flatten |> List.filter unsolved

    where
        unsolved C.Empty = False
        unsolved _       = True


flatten :: C.Constraint -> [C.Constraint]
flatten (Conjunction left right) = flatten left ++ flatten right
flatten c                        = pure c


instance Solve C.Constraint where
    solve (C.Equality ev it it') = solve $ E.Eq ev it it'
    solve (C.Impl ev it p)       = solve $ P.Impl ev it p
    solve c                      = return (Deferred, c)

    simplify (C.Equality ev it it') = simplify $ E.Eq ev it it'
    simplify (C.Impl ev it p)       = simplify $ P.Impl ev it p
    simplify c                      = return c

