module Saga.Language.Typechecker.Elaboration.Values.Effects where
import           Data.Map                                     (Map)
import qualified Data.Map                                     as Map

import           Effectful                                    ((:>))
import qualified Effectful.Reader.Static                      as Eff
import qualified Effectful.State.Static.Local                 as Eff
import qualified Effectful.Writer.Static.Local                as Eff
import qualified Saga.Language.Syntax.Elaborated.Types        as Elaborated
import           Saga.Language.Typechecker.Monad              (TypeCheck)
import qualified Saga.Language.Typechecker.Solver.Constraints as CST
import qualified Saga.Language.Typechecker.Variables          as Var
import           Saga.Language.Typechecker.Variables          (Variable)

type Elaboration es = (TypeCheck es, Eff.Reader Var.Level :> es, Eff.State State :> es, Eff.Writer CST.Constraint :> es)


data State = IST
  { tvars  :: Int
  , evars  :: Int
  , levels :: Map (Variable Elaborated.Type) Var.Level
  , proofs :: Map Elaborated.Type Elaborated.Type
  } deriving (Show)


initialState :: State
initialState = IST 0 0 Map.empty Map.empty
