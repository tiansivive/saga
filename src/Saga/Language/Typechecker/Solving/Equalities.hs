module Saga.Language.Typechecker.Solving.Equalities where


import qualified Data.Map                                            as Map
import           Effectful                                           (Eff, (:>))
import qualified Effectful.Error.Static                              as Eff
import qualified Effectful.State.Static.Local                        as Eff
import qualified Effectful.Writer.Static.Local                       as Eff


import qualified Saga.Language.Syntax.Elaborated.AST                 as EL

import qualified Saga.Language.Syntax.Elaborated.Types               as T
import           Saga.Language.Syntax.Elaborated.Types               (Type)

import           Saga.Language.Syntax.Polymorphism                   (Given (..),
                                                                      Polymorphic (..),
                                                                      Qualified (..))
import           Saga.Language.Typechecker.Errors                    (Exception (..),
                                                                      SagaError,
                                                                      crash)
import qualified Saga.Language.Typechecker.Solving.Constraints       as Solver hiding
                                                                               (evidence)
import           Saga.Language.Typechecker.Solving.Constraints       hiding
                                                                     (evidence)
import           Saga.Language.Typechecker.Solving.Cycles            (Cycle)
import           Saga.Language.Typechecker.Solving.Monad             (Proofs,
                                                                      Solution (..),
                                                                      Solving,
                                                                      Status (..))
import qualified Saga.Language.Typechecker.Solving.Shared            as Shared
import           Saga.Language.Typechecker.Solving.Shared            (Tag (..),
                                                                      update)

import           Data.Maybe                                          (isJust)
import           Debug.Pretty.Simple                                 (pTrace)


import           Saga.Language.Typechecker.Elaboration.Instantiation
import           Saga.Language.Typechecker.Elaboration.Monad         (Instantiate (..))
import           Saga.Language.Typechecker.Solving.Unification       (Unification (..))
import           Saga.Language.Typechecker.Substitution              (mkSubst)
import           Saga.Language.Typechecker.Variables                 (Variable)




solve :: Solving es => Solver.Constraint -> Eff es (Status, Solver.Constraint)

-- solve c | pTrace ("\n-------------------- SOLVING EQUALITY --------------------\n" ++ show c) False = undefined
solve (Solver.Equality _ (Solver.Ty ty) (Solver.Ty ty')) = do
    constraint <- Eff.execWriter @Solver.Constraint $ do
        t <- inst ty
        t' <- inst ty'
        unify' t t'

    return (Solved, constraint)

    where
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
            Eff.tell $ Solver.Equality ev (Solver.Ty $ T.Var tvar) (Solver.Ty fresh)
            inst instTy
        inst ty = return ty




solve c = crash $ Unexpected c "for Equality constraint"



simplify (Equality ev (Ty (arg `T.Arrow`  out)) (Ty (arg' `T.Arrow`  out'))) = do
    ev1 <- Shared.fresh Shared.E
    let inTypeEq = Solver.Equality ev1 (Ty $ EL.node arg) (Ty $ EL.node arg')

    -- ev2 <- Shared.fresh Shared.E
    -- let inKindEq = Solver.Equality ev2 (K $ EL.node $ EL.annotation arg) (K $ EL.node $ EL.annotation arg')

    ev3 <- Shared.fresh Shared.E
    let outTypeEq = Solver.Equality ev3 (Ty $ EL.node out) (Ty $ EL.node out')

    -- ev4 <- Shared.fresh Shared.E
    -- let outKindEq = Solver.Equality ev4 (K $ EL.node $ EL.annotation out) (K $ EL.node $ EL.annotation out')
    return $ Solver.Conjunction inTypeEq outTypeEq

simplify (Equality ev it it')
    | it == it' = do
        Shared.update E $ Map.fromList [(ev, Solver.Coercion Solver.Structural)]
        return Solver.Empty
    | otherwise = do
        Solution { evidence } <- Eff.get
        return $
            if isJust $ Map.lookup ev evidence then
                Solver.Empty
            else Solver.Equality ev it it'

