module Saga.Language.Typechecker.Solver.Equalities where
import qualified Effectful                                              as Eff
import qualified Effectful.State.Static.Local                           as Eff
import qualified Effectful.Writer.Static.Local                          as Eff
import           Prelude                                                hiding
                                                                        (Eq)

import qualified Saga.Language.Typechecker.Inference.Inference          as Inf
import           Saga.Language.Typechecker.Inference.Inference          (instantiateWith)
import           Saga.Language.Typechecker.Qualification                (Given (..),
                                                                         Qualified ((:=>)))
import qualified Saga.Language.Typechecker.Solver.Constraints           as C
import           Saga.Language.Typechecker.Solver.Constraints           (Evidence,
                                                                         Item (..))
import           Saga.Language.Typechecker.Solver.Monad
import qualified Saga.Language.Typechecker.Solver.Shared                as Shared
import           Saga.Language.Typechecker.Solver.Substitution          (compose,
                                                                         mkSubst)
import           Saga.Language.Typechecker.Solver.Unification           (Unification (..))
import qualified Saga.Language.Typechecker.Type                         as T
import           Saga.Language.Typechecker.Type                         (Scheme (..),
                                                                         Type)
import           Saga.Language.Typechecker.Variables                    (Variable)


import           Control.Monad                                          (foldM)
import qualified Data.Map                                               as Map
import           Data.Maybe                                             (isJust)
import           Debug.Pretty.Simple                                    (pTrace,
                                                                         pTraceM)
import           Effectful                                              (Eff)
import           Saga.Language.Typechecker.Errors                       (Exception (NotYetImplemented),
                                                                         crash)
import           Saga.Language.Typechecker.Inference.Type.Instantiation
import           Saga.Language.Typechecker.Solver.Cycles                (Cycle)
import qualified Saga.Language.Typechecker.Solver.Monad                 as Solve
import           Saga.Language.Typechecker.Solver.Protocols             (propagate)


data Eq = Eq (Variable Evidence) Item Item
    deriving Show


instance Solve Eq where
    solve = solve'
    simplify :: SolverEff es => Eq -> Eff es C.Constraint
    simplify = simplify'


solve' :: SolverEff es => Eq -> Eff es (Status, C.Constraint)
--solve' eq | pTrace ("\nSOLVING EQ:\n" ++ show eq) False = undefined
solve' (Eq _ it it') = case (it, it') of
    (Mono ty, Mono ty')                     -> ty `equals` ty'
    (Mono ty, Poly ty')                     -> instAndUnify ty' ty
    (Mono ty, Unification tvar)           -> ty `equals` T.Var tvar
    (Mono ty, Scoped tvar)                -> ty `equals` T.Var tvar

    (Poly ty, Mono ty')                     -> instAndUnify ty ty'
    (Poly ty, Unification tvar)           -> instAndUnify ty (T.Var tvar)

    (Unification tvar, Mono ty)           -> ty `equals` T.Var tvar
    (Unification tvar, Poly ty)           -> instAndUnify ty (T.Var tvar)
    (Unification tvar, C.Scoped tvar')    -> T.Var tvar `equals` T.Var tvar'

    (Scoped tvar, Unification tvar')    -> T.Var tvar `equals` T.Var tvar'
    (Scoped tvar, Mono ty)                -> ty `equals` T.Var tvar
    (Scoped tvar, Poly ty)                -> instAndUnify ty (T.Var tvar)

    eq -> crash $ NotYetImplemented $ "Solving equality: " ++ show eq

    where
        ty `equals`  ty' = unify' ty ty' >> return (Solved, C.Empty)

        instAndUnify poly ty = do
            (constraint, pt@(Forall [] (bs :| cs :=> qt))) <- inst poly
            unify' ty qt
            result <- propagate cs
            return (Solved, C.Conjunction constraint result)

        unify' ty ty' = do
            ((sub, cycles), proofs') <- Eff.runWriter @Proofs . Eff.runWriter @[Cycle Type]  $ unify ty ty'
            Eff.modify $ \sol -> sol{ proofs = proofs' `Map.union` proofs sol }
            Eff.modify $ mappend cycles
            update T sub

        inst ty@(Forall tvars _) = do
            (constraint, tvars') <- generate C.Empty [] tvars
            instTy <- instantiateWith ty tvars'
            return (constraint, instTy)

        generate constraint tys [] = return (constraint, reverse tys)
        generate constraint tys (tvar:tvars) = do
            ty <- T.Var <$> Shared.fresh Shared.T
            ev <-   Shared.fresh Shared.E

            let eq = C.Equality ev (Unification tvar) (Mono ty)
            generate (C.Conjunction constraint eq) (ty : tys) tvars



simplify' (Eq ev (Mono (arg `T.Arrow`  out)) (Mono (arg' `T.Arrow`  out'))) = do
    ev1 <- Shared.fresh Shared.E
    ev2 <- Shared.fresh Shared.E
    return $ C.Conjunction (C.Equality ev1 (Mono arg) (Mono arg')) (C.Equality ev2 (Mono out) (Mono out'))

simplify' (Eq ev it it')
    | it == it' = do
        update E $ mkSubst (ev, C.Coercion C.Structural)
        return C.Empty
    | otherwise = do
        Solution { evidence } <- Eff.get
        return $
            if isJust $ Map.lookup ev evidence then
                C.Empty
            else C.Equality ev it it'



