{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Typechecker.Solver.Refinements where
import           Control.Monad                                 (foldM, forM,
                                                                (>=>))
import           Control.Monad.State                           (MonadTrans (lift),
                                                                StateT (runStateT),
                                                                evalStateT)
import qualified Data.Map                                      as Map
import           Data.SBV                                      (SatResult (..),
                                                                runSMT, sNot,
                                                                sat, (.&&),
                                                                (.=>), (.>))
import qualified Data.SBV                                      as SBV

import           Control.Monad.IO.Class                        (MonadIO (..))
import qualified Data.Set                                      as Set
import           Debug.Pretty.Simple                           (pTraceM)
import           Effectful                                     (Eff)
import qualified Effectful                                     as Eff
import qualified Effectful.Error.Static                        as Eff
import           Saga.Language.Core.Literals                   (Literal (LBool, LInt))
import           Saga.Language.Typechecker.Environment         (CompilerState (assumptions))
import           Saga.Language.Typechecker.Errors              (SagaError (..))
import qualified Saga.Language.Typechecker.Refinement.Liquid   as L
import           Saga.Language.Typechecker.Refinement.Liquid   (Liquid)
import           Saga.Language.Typechecker.Refinement.SMT      (empty, prepare,
                                                                translate)
import qualified Saga.Language.Typechecker.Solver.Constraints  as C
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





data Refinement = Refine Scope Item Liquid deriving (Show)



instance Entails Refinement where
    -- | FIXME: Implement refinement entailment
    -- | SOLUTION: Use Z3 implication operator

    entails :: SolverEff es => Refinement -> [Constraint] -> Eff es [Constraint]
    entails ref@(Refine scope it liquid) cs = do
        pTraceM "\n\n---------------------\nCurrent: "
        pTraceM $ show ref
        pTraceM "Filtered: "
        pTraceM $ show filtered
        results <- Eff.liftIO $ SBV.runSMT solve'

        let implications = [ r | r@(SatResult (SBV.Unsatisfiable {})) <- results ]
        let models = [ model | r@(SatResult (SBV.Satisfiable _ model)) <- results ]

        pTraceM $ "\n\n----------\nModels:"
        pTraceM $ show models
        pTraceM $ "\n\n----------\nImplications:"
        pTraceM $ show results


        -- let (SatResult r) = result
        -- case r of
        --     SBV.Satisfiable _ model -> do
        --         pTraceM $ "\n\n----------\nModels:"
        --         pTraceM $ show model
        --         return cs
        --     SBV.Unsatisfiable {} -> return filtered
        if null implications then
            return cs
        else return filtered

        where
            solve' = solve >>= mapM (liftIO . SBV.sat)
            solve = do

                (st, assumptions) <- transform (empty, []) refinements
                pTraceM "\nAssumptions"
                pTraceM $ show assumptions
                proposition <- evalStateT (translate liquid) st
                forM assumptions $ \assump -> do
                    -- | Add an implication constraint
                    return $ assump .=> sNot proposition

            transform (env, ls) [] = do
                pTraceM "\nTransformed"


                pTraceM $ show env
                pTraceM $ show $ reverse ls
                return (env, reverse ls)

            transform (env, ls) (l:rest) = do
                (l', env') <- runStateT (translate l) env
                transform (env', l':ls) rest

            refinements = [ liq | C.Refined _ _ liq <- filtered ]
            filtered =  [ r | r@(C.Refined scope' it' liquid') <- cs
                         , liquid /= liquid'
                         ]



test = SBV.sat $ do
    x <- SBV.free "x"
    let gt10' = x .> SBV.literal (toInteger 10)
    let gt1' = x .> SBV.literal (toInteger 1)

    return $ gt10' .=> sNot gt1'



gt10 = C.Refined Map.empty (C.Mono T.Void) (L.Comparison L.GT (L.Var $ L.Poly "x") (L.Number 10))
gt1 = C.Refined Map.empty (C.Mono T.Void) (L.Comparison L.GT (L.Var $ L.Poly "x") (L.Number 1))




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

