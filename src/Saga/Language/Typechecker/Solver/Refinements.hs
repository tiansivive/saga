{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}
module Saga.Language.Typechecker.Solver.Refinements where
import qualified Data.Map                                      as Map
import           Data.SBV                                      (SatResult (..),
                                                                runSMT, sat)
import qualified Data.SBV                                      as SBV
import qualified Effectful                                     as Eff
import qualified Effectful.Error.Static                        as Eff
import           Saga.Language.Typechecker.Errors              (SagaError (..))
import qualified Saga.Language.Typechecker.Refinement.Liquid   as L
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


simplify' (Refine scope it liquid) = return $ Refined scope it liquid


-- | PROBLEM:Liquids #19 @tiansivive Liquid vars are strings, Substitution works with PolymorphicVars.
-- | SUGGESTION: Add a new Liquid PolymorphicVar?
instance Substitutable Liquid where

    -- | FIXME: I think the bindings have a mapping from vars to Item, so here we have to check if it's a singleton Type and translate to SMT
    type Target Liquid = Item

    apply s l@(L.Var v) = Map.findWithDefault l v s
