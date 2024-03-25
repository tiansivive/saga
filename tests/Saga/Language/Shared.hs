module Saga.Language.Shared where

import           Saga.Language.Syntax.AST
import           Saga.Language.Typechecker.Env
import           Test.Hspec

import qualified Data.Map                                      as Map

import qualified Saga.Language.Syntax.Elaborated.Types         as ET
import           Saga.Language.Typechecker.Elaboration.Effects (State (..))
import qualified Saga.Language.Typechecker.Solving.Constraints as Solver

check result action = case result of
    Left x -> expectationFailure $ "Expected Right value.\nGot: " ++ show x
    Right (Left err) -> expectationFailure $ "Expected Right value.\nGot: " ++ show err
    Right (Right res) -> action res


emptyEnv :: CompilerState Elaborated
emptyEnv = Saga [] Map.empty Map.empty Map.empty (Proofs ET.Void Map.empty)

emptyState :: State
emptyState = IST 0 0 0 Map.empty

simplify :: Solver.Constraint -> Solver.Constraint
simplify (Solver.Conjunction Solver.Empty b) = b
simplify (Solver.Conjunction a Solver.Empty) = a
simplify c                                   = c
