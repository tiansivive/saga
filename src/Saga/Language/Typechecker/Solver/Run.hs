
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

import           Saga.Language.Typechecker.Type                  (Polymorphic,
                                                                  Type)

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

import qualified Data.Set                                        as Set
import           Debug.Pretty.Simple                             (pTrace,
                                                                  pTraceM,
                                                                  pTraceShow)
import           Debug.Trace                                     (trace)
import qualified Effectful.Error.Static                          as Eff
import           Saga.Language.Typechecker.Inference.Type.Expr
import qualified Saga.Language.Typechecker.Monad                 as TC
import           Saga.Language.Typechecker.Monad                 (TypeCheck)
import qualified Saga.Language.Typechecker.Shared                as Shared
import           Saga.Language.Typechecker.Solver.Entailment     (Entails (..))

import           Effectful                                       (Eff)
import qualified Saga.Language.Typechecker.Solver.Implications   as Imp
import qualified Saga.Language.Typechecker.Solver.Refinements    as R
import           Saga.Language.Typechecker.Variables             (PolymorphicVar)
import           Saga.Language.Typechecker.Zonking.Normalisation (Normalisation (normalise))
import           Saga.Language.Typechecker.Zonking.Qualification (qualify)
import           Saga.Language.Typechecker.Zonking.Zonking       (Context (..),
                                                                  zonk)

run :: TypeCheck es => Constraint -> Eff es Context
run constraint = do
    pTraceM "Constraints:"
    pTraceM $ show (flatten constraint)
    ((residuals, cycles), solution) <- Eff.runState initialSolution . Eff.runState []  $ process (flatten constraint) []
    types <- foldM collapse (tvars solution) cycles


    return Context { solution = Solution { tvars = types, evidence = evidence solution, witnessed = witnessed solution, count = 0 }, residuals }

    where
        process :: SolverEff es => [C.Constraint] -> [(Status, C.Constraint)] -> Eff es [C.Constraint]
        --process cs done | pTrace ("\n----------------------\nPROCESSING:\n" ++ show cs ++ show done) False = undefined
        process [] done = let next = fmap snd done in
            if all deferred done then
                return next
            else process next []
        process cs done = do
            cs' <- step cs
            case cs' of
                (first:rest) -> do
                    attempt <- solve first
                    process rest (attempt:done)
                [] -> process [] done

        deferred (Deferred, _) = True
        deferred _             = False


step :: SolverEff es => [C.Constraint] -> Eff es [C.Constraint]
step cs = do
    Solution { tvars } <- Eff.get
    simplified <- forM cs $ \c -> simplify c
    entailment $ simplified ||> apply tvars >>= flatten |> List.filter unsolved

    where
        unsolved C.Empty = False
        unsolved _       = True


flatten :: C.Constraint -> [C.Constraint]
flatten (Conjunction left right) = flatten left ++ flatten right
flatten c                        = pure c


entailment :: SolverEff es =>  [Constraint] -> Eff es [Constraint]
entailment cs = List.nub <$> loop cs cs
    where

        loop [] done        = return done
        loop (c:cs') done   = do
            done' <- entails c done
            loop cs' done'


instance Entails Constraint where

    entails (C.Impl ev it p)            cs = entails (P.Impl ev it p) cs
    entails (C.Refined scope it liquid) cs = entails (R.Refine scope it liquid) cs
    entails _                           cs = return cs

instance Solve C.Constraint where
    solve (C.Equality ev it it')                    = solve $ E.Eq ev it it'
    solve (C.Impl ev it p)                          = solve $ P.Impl ev it p
    solve (C.Implication vars assumps constraint)   = solve $ Imp.Implies vars assumps constraint
    solve (C.Refined scope it liquid)               = solve $ R.Refine scope it liquid
    solve c                                         = return (Deferred, c)

    simplify (C.Equality ev it it')                      = simplify $ E.Eq ev it it'
    simplify (C.Impl ev it p)                            = simplify $ P.Impl ev it p
    simplify (C.Implication vars assumps constraint)     = simplify $ Imp.Implies vars assumps constraint
    simplify c                                           = return c

