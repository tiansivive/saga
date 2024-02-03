module Saga.Language.Typechecker.Solving.Implications where

import qualified Data.List                                     as List
import qualified Data.Map                                      as Map
import           Data.Maybe                                    (catMaybes)
import qualified Data.Set                                      as Set
import           Effectful                                     (Eff)
import qualified Effectful.Reader.Static                       as Eff
import qualified Saga.Language.Typechecker.Solving.Constraints as Solver
import           Saga.Language.Typechecker.Solving.Monad       (Levels, Solving)
import qualified Saga.Language.Typechecker.Solving.Shared      as Shared
import           Saga.Language.Typechecker.Substitution        (Substitutable (..))
import qualified Saga.Language.Typechecker.Variables           as Var
import           Saga.Utils.Operators                          ((|>), (||>))





simplify :: Solving es => Solver.Constraint -> Eff es Solver.Constraint
simplify (Solver.Implication vs as c) = do

    (ps, cs)  <- List.partition snd <$> mapM promote (Shared.flatten c)
    let ps' = fmap fst ps
    let cs' = fmap fst cs
    return $  Solver.Conjunction (Shared.merge ps') (Solver.Implication vs as $ Shared.merge cs')

    where
        promote :: Solving es => Solver.Constraint -> Eff es (Solver.Constraint, Bool)
        promote constraint = do
            lvls <- Eff.ask @Levels
            lvl <- Eff.ask @Var.Level
            let tvars = Set.toList $ ftv constraint
            if null tvars  then
                return (constraint, True)
            else do
                let max = constraint ||> (Set.toList . ftv) |> fmap (`Map.lookup` lvls) |> catMaybes |> List.maximum
                return (constraint, max < lvl)

