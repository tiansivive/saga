{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Typechecker.Solver.Refinements where
import           Control.Monad                                 (forM)
import qualified Data.Map                                      as Map
import           Data.SBV                                      (SatResult (..),
                                                                runSMT, sat)
import qualified Data.SBV                                      as SBV
import qualified Data.Set                                      as Set
import qualified Effectful                                     as Eff
import qualified Effectful.Error.Static                        as Eff
import           Saga.Language.Core.Literals                   (Literal (LBool, LInt))
import           Saga.Language.Typechecker.Errors              (SagaError (..))
import qualified Saga.Language.Typechecker.Refinement.Liquid   as L
import           Saga.Language.Typechecker.Refinement.Liquid   (Liquid)
import           Saga.Language.Typechecker.Refinement.SMT      (prepare)
import           Saga.Language.Typechecker.Solver.Constraints  (Constraint (Refined),
                                                                Item (..),
                                                                Scope)
import           Saga.Language.Typechecker.Solver.Entailment   (Entails (..))
import           Saga.Language.Typechecker.Solver.Monad        (Solve (..),
                                                                SolverEff,
                                                                SolverM,
                                                                Status (..))
import           Saga.Language.Typechecker.Solver.Substitution (Substitutable (..))
import qualified Saga.Language.Typechecker.Type                as T
import           Saga.Utils.Operators                          ((||>))
import qualified Saga.Utils.TypeLevel                          as PolymorphicVars




data Refinement = Refine Scope Item Liquid



instance Entails Refinement where
    -- | FIXME: Implement refinement entailment
    -- | SOLUTION: Generate implication constraints



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


simplify' :: Refinement -> SolverM Constraint
simplify' (Refine scope it liquid) = do
    let subst = scope ||> Map.mapWithKey convert
    return $ Refined scope it (apply subst liquid)

    where
        convert _ (Mono (T.Singleton lit))  | LInt n <- lit     = L.Number n
                                            | LBool b <- lit    = L.Boolean b
        convert var _                                           = L.Var var


instance Substitutable Liquid where

    -- | FIXME: I think the bindings have a mapping from vars to Item, so here we have to check if it's a singleton Type and translate to SMT
    type Target Liquid = Liquid

    apply s l@(L.Var v)           = Map.findWithDefault l v s
    apply s (L.Arithmetic op l r) = L.Arithmetic op (apply s l) (apply s r)
    apply s (L.Comparison op l r) = L.Comparison op (apply s l) (apply s r)
    apply s (L.Logical op l r)    = L.Logical op (apply s l) (apply s r)
    apply s (L.Equality l r)      = L.Equality (apply s l) (apply s r)
    apply s (L.Negation l )       = L.Negation (apply s l)
    apply _ l                     = l

    ftv (L.Var v)            = Set.singleton v
    ftv (L.Arithmetic _ l r) = ftv l <> ftv r
    ftv (L.Comparison _ l r) = ftv l <> ftv r
    ftv (L.Logical _ l r)    = ftv l <> ftv r
    ftv (L.Equality l r)     = ftv l <> ftv r
    ftv (L.Negation l)       = ftv l
    ftv _                    = Set.empty

