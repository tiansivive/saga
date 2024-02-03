{-# LANGUAGE LambdaCase #-}
module Saga.Language.Typechecker.Solving.Run where
import           Control.Monad                                 (foldM, forM,
                                                                forM_)
import qualified Data.List                                     as List
import qualified Data.Map                                      as Map
import           Debug.Pretty.Simple                           (pTrace, pTraceM)
import           Effectful                                     (Eff)
import qualified Effectful.Reader.Static                       as Eff
import qualified Effectful.State.Static.Local                  as Eff
import qualified Saga.Language.Syntax.Elaborated.Types         as T
import qualified Saga.Language.Typechecker.Solving.Constraints as Solver
import           Saga.Language.Typechecker.Solving.Constraints (Constraint)
import           Saga.Language.Typechecker.Solving.Cycles      (collapse)
import qualified Saga.Language.Typechecker.Solving.Equalities  as Equalities
import           Saga.Language.Typechecker.Solving.Monad       (Entails (..),
                                                                Levels,
                                                                Solution (..),
                                                                Solve (..),
                                                                Solving,
                                                                Status (..))
import qualified Saga.Language.Typechecker.Solving.Protocols   as Protocols
import qualified Saga.Language.Typechecker.Solving.Shared      as Shared
import           Saga.Language.Typechecker.Substitution        (Substitutable (..))
import qualified Saga.Language.Typechecker.Variables           as Var

import           Saga.Utils.Operators                          ((|>), (||>))




run :: Solving es => (Constraint, Levels) -> Eff es [Solver.Constraint]
run (constraint, lvls) = do
    -- pTraceM $ "\nCONSTRAINTS:\n" ++ show (Shared.flatten constraint)
    -- ((residuals, cycles), solution) <- Eff.runState initialSolution . Eff.evalState initialCount .  Eff.runState [] . Eff.runReader (Var.Level 0) . Eff.runReader levels $ process (Shared.flatten constraint) []
    -- pTraceM $ "\n-----------------------------\nCYCLES:\n" ++ show cycles
    -- pTraceM $ "\n-----------------------------\nRESIDUALS:\n" ++ show residuals
    -- pTraceM $ "\n-----------------------------\nSOLUTION:\n" ++ show solution
    -- types <- foldM collapse (tvars solution) cycles


    -- return Context { solution = Solution { tvars = types, evidence = evidence solution, witnessed = witnessed solution, proofs = proofs solution }, residuals }
    process (Shared.flatten constraint) []
    where
        process :: Solving es => [Solver.Constraint] -> [(Status, Solver.Constraint)] -> Eff es [Solver.Constraint]
        -- process cs done | pTrace ("\n----------------------\nPROCESSING:\n" ++ show cs ++ show done) False = undefined
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


step :: Solving es => [Solver.Constraint] -> Eff es [Solver.Constraint]
-- step cs | pTrace ("\n----------------------\nSTEP:\n" ++ show cs) False = undefined
step cs = do
    Solution { tvars, proofs } <- Eff.get
    simplified <- forM cs simplify
    forM_ simplified $ propagate proofs
    entailment $ simplified ||> apply tvars >>= Shared.flatten |> List.filter unsolved

    where
        propagate proofs (Solver.Conjunction left right) = propagate proofs left >> propagate proofs right
        propagate proofs (Solver.Equality ev ty ty') = let
            tvar = \case
                Solver.Ty (T.Var tv) -> Just tv
                _                    -> Nothing
            in case (tvar ty, tvar ty') of
                (Just tvar, Just tvar')
                    | Just proof <- Map.lookup tvar  proofs
                        -> Eff.modify $ \sol -> sol{ proofs = Map.insert tvar' proof proofs }
                    | Just proof <- Map.lookup tvar' proofs
                        -> Eff.modify $ \sol -> sol{ proofs = Map.insert tvar  proof proofs }
                    | otherwise -> return ()
                _ -> return ()

        propagate _ _ = return ()

        unsolved Solver.Empty = False
        unsolved _            = True





entailment :: Solving es =>  [Constraint] -> Eff es [Constraint]
entailment cs = List.nub <$> loop cs cs
    where

        loop [] done        = return done
        loop (c:cs') done   = do
            done' <- entails c done
            loop cs' done'





instance Entails Constraint where

    -- entails (Solver.Impl ev it p)            cs = entails (P.Impl ev it p) cs
    -- entails (Solver.Refined scope it liquid) cs = entails (R.Refine scope it liquid) cs
    entails _ = return

-- -- | SUGGESTION: This is a good place to start refactoring the constraints into a data family.
-- -- | That way we do not need to pattern match on the constructors here, we can just have a different instance for each constructor.
instance Solve Solver.Constraint where
    solve c@(Solver.Equality {})       = Equalities.solve c
    solve c@(Solver.Implementation {}) = Protocols.solve c


    solve (Solver.Implication vs as Solver.Empty) = return (Deferred, Shared.merge cs)
        where cs = [c | Solver.Assume c <- as]

    solve (Solver.Implication vs as c) = Eff.local @Var.Level (+1) $ do
        lvls <- Eff.ask @Levels
        residuals <- run (c, lvls)
        let cs = [ c | Solver.Assume c <- as ]
        return (Deferred, Shared.merge $ cs ++ residuals)


    -- solve (Solver.Implication vars assumps constraint)   = solveImplication $ Imp.Implies vars assumps constraint
    -- solve (Solver.Refined scope it liquid)               = solve $ R.Refine scope it liquid
    -- solve Solver.Empty                                   = return (Solved, Solver.Empty)
    --solve c@(Solver.Conjunction {})                      = return (Deferred, c)
    solve c                            = return (Deferred, c)
    -- solve c                                         = crash $ NotYetImplemented $ "Solving constraint: " ++ show c

    simplify :: Solving es => Constraint -> Eff es Constraint
    simplify c@(Solver.Equality ev it it') = Equalities.simplify c
    -- simplify (Solver.Impl ev it p)                            = simplify $ P.Impl ev it p
    -- simplify (Solver.Refined scope it liquid)                 = simplify $ R.Refine scope it liquid
    -- simplify (Solver.Implication vars assumps constraint)     = Imp.simplify' $ Imp.Implies vars assumps constraint
    simplify c@(Solver.Conjunction {})     = return c
    simplify c@Solver.Empty                = return c
    simplify c                             = return c
    --simplify c                                           = crash $ NotYetImplemented $ "Simplifying constraint: " ++ show c

-- -- FIXME: #35 This needs to be moved to its own file. It's only here to prevent cyclic import dependencies.
-- solveImplication :: (SolverEff es, Solve Constraint) => Imp.Implies -> Eff es (Status, Constraint)
-- solveImplication (Imp.Implies vs as Solver.Empty) = return (Deferred, Shared.merge cs)
--     where cs = [c | Solver.Assume c <- as]

-- solveImplication (Imp.Implies vs as c) = Eff.local @Var.Level (+1) $ do
--     lvls <- Eff.ask @Levels
--     Context { residuals } <- run (c, lvls)
--     let cs = [c | Solver.Assume c <- as]
--     return (Deferred, Shared.merge $ cs ++ residuals)
