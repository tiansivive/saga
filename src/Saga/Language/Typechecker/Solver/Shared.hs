module Saga.Language.Typechecker.Solver.Shared where

import           Control.Monad.RWS                            (gets, modify)

import qualified Saga.Language.Typechecker.Qualification      as Q
import qualified Saga.Language.Typechecker.Solver.Constraints as C
import           Saga.Language.Typechecker.Solver.Constraints (Constraint (..),
                                                               Evidence,
                                                               Item (..))
import           Saga.Language.Typechecker.Solver.Monad       (SolverM,
                                                               State (..))

import qualified Effectful.State.Static.Local                 as Eff
import           Saga.Language.Typechecker.Type               (Type)
import qualified Saga.Language.Typechecker.Variables          as Var
import           Saga.Language.Typechecker.Variables          (PolymorphicVar)



from :: Q.Constraint Type -> SolverM Constraint
from (Q.Pure t)          = return $ C.Pure $ Mono t
from (Q.Resource m t)    = return $ C.Resource (Mono t) m
from (Q.Refinement re t) = return $ C.Refined (Mono t) re
from (Q.Implements t p)  = do
    superEv <- fresh
    return $ C.Impl superEv (Mono t) p
from (Q.Equality t t')   = do
    eqEv <- fresh
    return $ C.Equality eqEv (Mono t) (Mono t')




fresh :: SolverM (PolymorphicVar Evidence)
fresh = do
  Eff.modify $ \s -> s {count = count s + 1}
  index <- Eff.gets count
  let count = show ([1 ..] !! index)
  return $ Var.Evidence $ "cst_ev_" ++ count
