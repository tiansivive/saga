{-# LANGUAGE TypeFamilies #-}
module Saga.Language.Typechecker.Solver.Entailment where
import           Control.Monad                                (forM)
import           Data.Functor                                 ((<&>))
import qualified Data.List                                    as List
import qualified Data.Map                                     as Map
import           Data.Maybe                                   (catMaybes,
                                                               isJust)
import           Debug.Pretty.Simple                          (pTraceM)
import qualified Effectful.Error.Static                       as Eff
import qualified Effectful.State.Static.Local                 as Eff
import           Saga.Language.Typechecker.Errors             (SagaError (..))
import qualified Saga.Language.Typechecker.Solver.Constraints as C
import           Saga.Language.Typechecker.Solver.Constraints (Constraint)
import           Saga.Language.Typechecker.Solver.Monad       (Solution (..),
                                                               SolverM)
import           Saga.Utils.Operators                         ((|>), (||>))



-- | ISSUE #23
-- | TODO #24 Move Constraint list to a state effect within the Solver monad
class Entails a where
    entails :: a -> [Constraint] -> SolverM [Constraint]




-- entails :: Constraint -> [Constraint] -> SolverM [Constraint]
-- entails i@(C.Impl e it pid) cs = do
--     env@Solution { evidence, witnessed } <- Eff.get

--     evar <- case exists evidence of
--         []       -> return e
--         [evar]   -> return evar
--         multiple -> Eff.throwError $ MultipleImplementationEvidence it pid

--     Eff.modify @Solution $ \s -> s { witnessed = witnessed `Map.union` next witnessed evar }
--     return $ fmap (normalize evars evar) cs

--     where
--         next ws e = evars ||>List.filter (\e -> e `notElem` Map.keys ws ) |> fmap (,e) |> Map.fromList
--         exists evidence = evars ||> List.filter (\var -> isJust $ Map.lookup var evidence)

--         evars = fmap snd impls
--         impls = [ (c, e') | c@(C.Impl e' it' pid') <- cs
--                     , it == it'
--                     , pid == pid'
--                     , e /= e'

--                 ]

--         normalize es e (C.Impl e' it' pid') | e' `elem` es   = C.Impl e it' pid'
--         normalize _ _ constraint                             = constraint

-- entails _ cs = return cs
