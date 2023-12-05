{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Typechecker.Solver.Refinements where
import           Control.Monad                                 (forM)
import qualified Data.Map                                      as Map
import           Data.SBV                                      (SatResult (..),
                                                                runSMT, sNot,
                                                                sat, (.=>))
import qualified Data.SBV                                      as SBV
import qualified Data.Set                                      as Set
import           Effectful                                     (Eff)
import qualified Effectful                                     as Eff
import qualified Effectful.Error.Static                        as Eff
import           Saga.Language.Core.Literals                   (Literal (LBool, LInt))
import           Saga.Language.Typechecker.Errors              (SagaError (..))
import qualified Saga.Language.Typechecker.Refinement.Liquid   as L
import           Saga.Language.Typechecker.Refinement.Liquid   (Liquid)
import           Saga.Language.Typechecker.Refinement.SMT      (prepare)
import           Saga.Language.Typechecker.Solver.Constraints  (Constraint (Empty, Refined),
                                                                Item (..),
                                                                Scope)
import           Saga.Language.Typechecker.Solver.Entailment   (Entails (..))
import           Saga.Language.Typechecker.Solver.Monad        (Solve (..),
                                                                SolverEff,
                                                                Status (..))
import           Saga.Language.Typechecker.Solver.Substitution (Substitutable (..))
import qualified Saga.Language.Typechecker.Type                as T
import           Saga.Utils.Operators                          ((||>))
import qualified Saga.Utils.TypeLevel                          as PolymorphicVars




data Refinement = Refine Scope Item Liquid



instance Entails Refinement where
    -- | FIXME: Implement refinement entailment
    -- | SOLUTION: Use Z3 implication operator

    entails (Refine scope it liquid) cs = do
        results <- mapM implied rest
        if any (\(SatResult (SBV.Unsatisfiable {})) -> True) results then
            return $ filter ()
        else return cs
        where
            rest = [ prepare liq | Refined _ _ liq <- cs ]
            implied other = Eff.liftIO . SBV.sat $ do

                current <- prepare liquid
                assumption <- other
                -- -- Add an implication constraint
                return $ assumption .=> sNot current






instance Solve Refinement where

    solve = solve'
    simplify = simplify'


-- | TODO: #30 Generate proofs/witnesses by leveraging the evidence system. This is how to identify that a certain type has been refined/narrowed
solve' :: SolverEff es => Refinement -> Eff es (Status, Constraint)
solve' r@(Refine scope it liquid) = do
    res <- Eff.liftIO $ SBV.sat (prepare liquid)
    case res of
        SatResult (SBV.Satisfiable _ model) -> return $
            if null $ ftv liquid then
                (Solved, Empty)
            else (Deferred, Refined scope it liquid)
        _ -> Eff.throwError $ UnsatisfiableRefinement $ Refined scope it liquid


simplify' :: SolverEff es => Refinement -> Eff es  Constraint
simplify' (Refine scope it liquid) = do
    let subst = scope ||> Map.mapWithKey convert
    return $ Refined scope it (apply subst liquid)

    where
        convert _ (Mono (T.Singleton lit))  | LInt n <- lit     = L.Number n
                                            | LBool b <- lit    = L.Boolean b
        convert var _                                           = L.Var var


instance Substitutable Liquid where
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

