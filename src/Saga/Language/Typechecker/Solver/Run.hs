{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Typechecker.Solver.Run where

import           Control.Monad                                   (foldM, forM,
                                                                  forM_)
import qualified Effectful                                       as Eff
import qualified Effectful.State.Static.Local                    as Eff
import qualified Effectful.Writer.Static.Local                   as Eff
import           Saga.Language.Core.Expr                         (Expr)
import           Saga.Language.Typechecker.Inference.Type.Shared (State (levels),
                                                                  TypeInference)
import qualified Saga.Language.Typechecker.Solver.Constraints    as C
import           Saga.Language.Typechecker.Solver.Constraints    (Constraint (Conjunction))
import qualified Saga.Language.Typechecker.Solver.Monad          as Solve hiding
                                                                          (run)
import           Saga.Language.Typechecker.Solver.Monad          hiding (run)
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
import           Saga.Language.Typechecker.Inference.Type.Expr   hiding (run)
import qualified Saga.Language.Typechecker.Monad                 as TC
import           Saga.Language.Typechecker.Monad                 (TypeCheck)
import qualified Saga.Language.Typechecker.Shared                as Shared
import           Saga.Language.Typechecker.Solver.Entailment     (Entails (..))

import           Data.Maybe                                      (catMaybes)
import           Effectful                                       (Eff)
import           Saga.Language.Typechecker.Errors                (Exception (..),
                                                                  crash)
import qualified Saga.Language.Typechecker.Solver.Implications   as Imp
import qualified Saga.Language.Typechecker.Solver.Refinements    as R
import qualified Saga.Language.Typechecker.Solver.Shared         as Shared
import qualified Saga.Language.Typechecker.Variables             as Var
import           Saga.Language.Typechecker.Variables             (Variable)
import           Saga.Language.Typechecker.Zonking.Normalisation (Normalisation (normalise))
import           Saga.Language.Typechecker.Zonking.Qualification (qualify)
import           Saga.Language.Typechecker.Zonking.Zonking       (Context (..),
                                                                  zonk)

run :: TypeCheck es => (Constraint, Levels) -> Eff es Context
run (constraint, levels) = do
    pTraceM $ "\nCONSTRAINTS:\n" ++ show (Shared.flatten constraint)
    ((residuals, cycles), solution) <- Eff.runState initialSolution . Eff.evalState initialCount .  Eff.runState [] . Eff.runReader (Var.Level 0) . Eff.runReader levels $ process (Shared.flatten constraint) []
    pTraceM $ "\n-----------------------------\nCYCLES:\n" ++ show cycles
    pTraceM $ "\n-----------------------------\nRESIDUALS:\n" ++ show residuals
    pTraceM $ "\n-----------------------------\nSOLUTION:\n" ++ show solution
    types <- foldM collapse (tvars solution) cycles


    return Context { solution = Solution { tvars = types, evidence = evidence solution, witnessed = witnessed solution, proofs = proofs solution }, residuals }

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
    Solution { tvars, proofs } <- Eff.get
    simplified <- forM cs simplify
    forM_ simplified $ propagate proofs
    entailment $ simplified ||> apply tvars >>= Shared.flatten |> List.filter unsolved

    where
        propagate proofs (C.Conjunction left right) = propagate proofs left >> propagate proofs right
        propagate proofs (C.Equality ev it it') = let
            tvar = \case
                C.Var tv -> Just tv
                _        -> Nothing
            in case (tvar it, tvar it') of
                (Just tvar, Just tvar')
                    | Just proof <- Map.lookup tvar  proofs
                        -> Eff.modify $ \sol -> sol{ proofs = Map.insert tvar' proof proofs }
                    | Just proof <- Map.lookup tvar' proofs
                        -> Eff.modify $ \sol -> sol{ proofs = Map.insert tvar  proof proofs }
                    | otherwise -> return ()
                _ -> return ()

        propagate _ _ = return ()

        unsolved C.Empty = False
        unsolved _       = True





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

-- | SUGGESTION: This is a good place to start refactoring the constraints into a data family.
-- | That way we do not need to pattern match on the constructors here, we can just have a different instance for each constructor.
instance Solve C.Constraint where
    solve (C.Equality ev it it')                    = solve $ E.Eq ev it it'
    solve (C.Impl ev it p)                          = solve $ P.Impl ev it p
    solve (C.Implication vars assumps constraint)   = solveImplication $ Imp.Implies vars assumps constraint
    solve (C.Refined scope it liquid)               = solve $ R.Refine scope it liquid
    solve C.Empty                                   = return (Solved, C.Empty)
    solve c@(C.Conjunction {})                      = return (Deferred, c)
    solve c                                         = crash $ NotYetImplemented $ "Solving constraint: " ++ show c

    simplify :: SolverEff es => Constraint -> Eff es Constraint
    simplify (C.Equality ev it it')                      = simplify $ E.Eq ev it it'
    simplify (C.Impl ev it p)                            = simplify $ P.Impl ev it p
    simplify (C.Refined scope it liquid)                 = simplify $ R.Refine scope it liquid
    simplify (C.Implication vars assumps constraint)     = Imp.simplify' $ Imp.Implies vars assumps constraint
    simplify c@(C.Conjunction {})                        = return c
    simplify c@C.Empty                                   = return c
    simplify c                                           = crash $ NotYetImplemented $ "Simplifying constraint: " ++ show c

-- FIXME: #35 This needs to be moved to its own file. It's only here to prevent cyclic import dependencies.
solveImplication :: (SolverEff es, Solve Constraint) => Imp.Implies -> Eff es (Status, Constraint)
solveImplication (Imp.Implies vs as C.Empty) = return (Deferred, Shared.merge cs)
    where cs = [c | C.Assume c <- as]

solveImplication (Imp.Implies vs as c) = Eff.local @Var.Level (+1) $ do
    lvls <- Eff.ask @Levels
    Context { residuals } <- run (c, lvls)
    let cs = [c | C.Assume c <- as]
    return (Deferred, Shared.merge $ cs ++ residuals)
