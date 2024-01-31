{-# LANGUAGE DataKinds #-}

module Saga.Language.Typechecker.Elaboration.Effects where
import           Data.Map                                      (Map)
import qualified Data.Map                                      as Map

import           Effectful                                     ((:>))

import qualified Effectful.Error.Static                        as Eff
import qualified Effectful.Fail                                as Eff
import qualified Effectful.Reader.Static                       as Eff
import qualified Effectful.State.Static.Local                  as Eff
import qualified Effectful.Writer.Static.Local                 as Eff
import           Saga.Language.Syntax.AST                      (Phase (Elaborated))
import qualified Saga.Language.Syntax.Elaborated.Types         as Elaborated
import           Saga.Language.Typechecker.Env                 (CompilerState,
                                                                Info)
import           Saga.Language.Typechecker.Errors              (SagaError)
import qualified Saga.Language.Typechecker.Solving.Constraints as CST
import qualified Saga.Language.Typechecker.Variables           as Var
import           Saga.Language.Typechecker.Variables           (Variable)

type Elaboration es = (Eff.Reader (CompilerState Elaborated) :> es, Eff.Writer Info :> es, Eff.Error SagaError :> es, Eff.Fail :> es, Eff.Reader Var.Level :> es, Eff.State State :> es, Eff.Writer CST.Constraint :> es)


data State = IST
  { tvars  :: Int
  , kvars  :: Int
  , evars  :: Int
  , levels :: Map (Variable Elaborated.Type) Var.Level
  } deriving (Show)


initialState :: State
initialState = IST 0 0 0 Map.empty
