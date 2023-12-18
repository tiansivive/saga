module Saga.Language.Typechecker.Solver.Implications where
import           Effectful                                       (Eff)
import qualified Saga.Language.Typechecker.Solver.Constraints    as C

import           Control.Monad                                   (filterM)
import qualified Data.List                                       as List
import qualified Data.Map                                        as Map
import           Data.Maybe                                      (catMaybes)
import qualified Data.Set                                        as Set
import qualified Effectful.Reader.Static                         as Eff
import qualified Effectful.State.Static.Local                    as Eff
import           Saga.Language.Typechecker.Inference.Type.Shared (State (..))
import           Saga.Language.Typechecker.Solver.Constraints    (Assumption,
                                                                  Constraint)
import           Saga.Language.Typechecker.Solver.Monad
import qualified Saga.Language.Typechecker.Solver.Shared         as Shared
import           Saga.Language.Typechecker.Solver.Substitution   (Substitutable (..))
import qualified Saga.Language.Typechecker.Type                  as T
import           Saga.Language.Typechecker.Type                  (Type)
import qualified Saga.Language.Typechecker.Variables             as Var
import           Saga.Language.Typechecker.Variables             (Variable)
import           Saga.Utils.Operators                            ((|>), (||>))



data Implies = Implies [Variable Type] [Assumption] Constraint

-- instance Solve Implies where
--     solve = solve'
--     simplify = simplify'




simplify' :: SolverEff es => Implies -> Eff es Constraint
simplify' (Implies vs as c) = do

    (ps, cs)  <- List.partition snd <$> mapM promote (Shared.flatten c)
    let ps' = fmap fst ps
    let cs' = fmap fst cs
    return $  C.Conjunction (Shared.merge ps') (C.Implication vs as $ Shared.merge cs')

    where
        promote :: SolverEff es => Constraint -> Eff es (Constraint, Bool)
        promote constraint = do
            lvls <- Eff.ask @Levels
            lvl <- Eff.ask @Var.Level
            let tvars = Set.toList $ ftv constraint
            if null tvars  then
                return (constraint, True)
            else do
                let max = constraint ||> (Set.toList . ftv) |> fmap (`Map.lookup` lvls) |> catMaybes |> List.maximum
                return (constraint, max < lvl)

