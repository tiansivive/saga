{-# LANGUAGE DataKinds #-}
module Saga.Language.Typechecker.Solver.Refinements where
import           Data.SBV                                      (SatResult (..),
                                                                runSMT, sat)
import qualified Data.SBV                                      as SBV
import qualified Effectful                                     as Eff
import qualified Effectful.Error.Static                        as Eff
import           Saga.Language.Typechecker.Errors              (SagaError (..))
import           Saga.Language.Typechecker.Refinement.Liquid   (Liquid)
import           Saga.Language.Typechecker.Refinement.SMT      (prepare)
import           Saga.Language.Typechecker.Solver.Constraints  (Constraint (Refined),
                                                                Item, Scope)
import           Saga.Language.Typechecker.Solver.Entailment   (Entails (..))
import           Saga.Language.Typechecker.Solver.Monad        (Solve (..),
                                                                SolverEff,
                                                                SolverM,
                                                                Status (..))
import           Saga.Language.Typechecker.Solver.Substitution (Substitutable (..))
import qualified Saga.Utils.TypeLevel                          as PolymorphicVars




data Refinement = Refine Scope Item Liquid



instance Entails Refinement where
    -- | FIXME: Implement refinement entailment
    -- | SOLUTION: Generate implication constraints
    entails r = return



instance Solve Refinement where

    solve = solve'
    simplify = simplify'



solve' :: Refinement -> SolverM (Status, Constraint)
solve' r@(Refine scope it liquid) = do

    res <- Eff.liftIO $ SBV.sat (prepare liquid)
    case res of
        SatResult (SBV.Satisfiable _ model) -> do
            return (Deferred, Refined scope it liquid)
        _ -> Eff.throwError $ UnsatisfiableRefinement $ Refined scope it liquid


simplify' r = do
    _r


-- | PROBLEM:Liquids #19 @tiansivive Liquid vars are strings, Substitution works with PolymorphicVars.
-- | SUGGESTION: Add a new Liquid PolymorphicVar?
instance Substitutable Liquid where
    apply sub = _p
