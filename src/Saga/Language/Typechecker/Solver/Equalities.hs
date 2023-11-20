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
import           Saga.Language.Typechecker.Variables                    (PolymorphicVar)


import           Control.Monad                                          (foldM)
import qualified Data.Map                                               as Map
import           Data.Maybe                                             (isJust)
import           Debug.Pretty.Simple                                    (pTrace,
                                                                         pTraceM)
import           Saga.Language.Typechecker.Errors                       (Exception (NotYetImplemented),
                                                                         crash)
import           Saga.Language.Typechecker.Inference.Type.Instantiation
import           Saga.Language.Typechecker.Solver.Cycles                (Cycle)
import qualified Saga.Language.Typechecker.Solver.Monad                 as Solve
import           Saga.Language.Typechecker.Solver.Protocols             (propagate)


data Eq = Eq (PolymorphicVar Evidence) Item Item
    deriving Show


instance Solve Eq where
    solve = solve'
    simplify = simplify'


solve' :: Eq -> SolverM (Status, C.Constraint)
--solve' eq | pTrace ("\nSOLVING EQ:\n" ++ show eq) False = undefined
solve' (Eq _ it it') = case (it, it') of
    (Mono ty, Mono ty')        -> ty `equals` ty'
    (Mono ty, Unification var) -> ty `equals` T.Var var
    (Unification var, Mono ty) -> ty `equals` T.Var var
    (Mono ty, Poly ty')        -> instAndUnify ty' ty
    (Poly ty', Mono ty)        -> instAndUnify ty' ty
    eq -> crash $ NotYetImplemented $ "Solving equality: " ++ show eq

    where
        ty `equals`  ty' = unify' ty ty' >> return (Solved, C.Empty)

        instAndUnify poly ty = do
            (constraint, pt@(Forall [] (bs :| cs :=> qt))) <- inst poly
            unify' ty qt
            result <- propagate cs
            return (Solved, C.Conjunction constraint result)

        unify' ty ty' = do
            (sub, cycles) <- Eff.runWriter @[Cycle Type] $ Eff.inject $ unify ty ty'
            Eff.modify $ mappend cycles
            update T sub

        inst ty@(Forall tvars _) = do
            (constraint, tvars') <- generate C.Empty [] tvars
            instTy <- instantiateWith ty tvars'
            return (constraint, instTy)

        generate constraint tys [] = return (constraint, reverse tys)
        generate constraint tys (tvar:tvars) = do
            ty <- T.Var <$> Shared.fresh Inf.U
            ev <- Shared.fresh Inf.E
            let eq = C.Equality ev (Mono $ T.Var tvar) (Mono ty)
            generate (C.Conjunction constraint eq) (ty : tys) tvars


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



