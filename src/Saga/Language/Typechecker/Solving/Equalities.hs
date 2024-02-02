module Saga.Language.Typechecker.Solving.Equalities where


import qualified Data.Map                                            as Map
import           Effectful                                           (Eff, (:>))
import qualified Effectful.State.Static.Local                        as Eff
import qualified Effectful.Writer.Static.Local                       as Eff
import qualified Saga.Language.Syntax.Elaborated.Types               as T
import           Saga.Language.Syntax.Elaborated.Types               (Type)
import           Saga.Language.Syntax.Polymorphism                   (Given (..),
                                                                      Polymorphic (..),
                                                                      Qualified (..))
import           Saga.Language.Typechecker.Errors                    (Exception (..),
                                                                      SagaError,
                                                                      crash)
import qualified Saga.Language.Typechecker.Solving.Constraints       as Solver
import           Saga.Language.Typechecker.Solving.Constraints
import           Saga.Language.Typechecker.Solving.Cycles            (Cycle)
import           Saga.Language.Typechecker.Solving.Monad             (Proofs,
                                                                      Solution (..),
                                                                      Solving,
                                                                      Status (..))
import qualified Saga.Language.Typechecker.Solving.Shared            as Shared
import           Saga.Language.Typechecker.Solving.Shared            (Tag (..))

import qualified Effectful.Error.Static                              as Eff
import           Saga.Language.Typechecker.Elaboration.Instantiation
import           Saga.Language.Typechecker.Elaboration.Monad         (Instantiate (..))
import           Saga.Language.Typechecker.Variables                 (Variable)
import Saga.Language.Typechecker.Solving.Unification (Unification(..))




solve :: Solving es => Solver.Constraint -> Eff es (Status, Solver.Constraint)

solve (Solver.Equality _ it it') = case (it, it') of
    (Mono ty, Mono ty')                     -> ty `equals` ty'
    (Mono ty, Poly ty')                     -> instAndUnify ty' ty
    (Mono ty, Var tvar)           -> ty `equals` T.Var tvar


    (Poly ty, Mono ty')                     -> instAndUnify ty ty'
    (Poly ty, Var tvar)           -> instAndUnify ty (T.Var tvar)

    (Var tvar, Mono ty)           -> ty `equals` T.Var tvar
    (Var tvar, Poly ty)           -> instAndUnify ty (T.Var tvar)
    (Var tvar, Var tvar')         -> T.Var tvar `equals` T.Var tvar'

    eq -> crash $ NotYetImplemented $ "Solving equality: " ++ show eq

    where
        ty `equals`  ty' = unify' ty ty' >> return (Solved, Solver.Empty)

        instAndUnify poly ty = do
            (t, constraint) <- Eff.runWriter $ inst poly
            unify' ty t
            case t of
                T.Qualified (bs :| cs :=> qt) -> do
                    result <- Shared.propagate cs
                    return (Solved, Solver.Conjunction constraint result)
                _ -> return (Solved, constraint)


        unify' ty ty' = do
            ((sub, cycles), proofs') <- Eff.runWriter @Proofs . Eff.runWriter @[Cycle Type]  $ unify ty ty'
            Eff.modify $ \sol -> sol{ proofs = proofs' `Map.union` proofs sol }
            Eff.modify $ mappend cycles
            Shared.update T sub


        inst (T.Polymorphic poly@(Forall [] _)) = crash $ Unexpected poly "No type variables to instantiate polymorphic type"
        inst (T.Polymorphic poly@(Forall (tvar:tvars) _)) = do
            fresh <- T.Var <$> Shared.fresh Shared.T
            let instTy = instantiate poly fresh
            ev <- Shared.fresh Shared.E
            Eff.tell $ Solver.Equality ev (Var tvar) (Mono fresh)
            inst instTy
        inst ty = return ty




solve c = crash $ Unexpected c "for Equality constraint"

