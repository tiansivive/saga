{-# LANGUAGE TypeFamilies #-}
module Saga.Language.Typechecker.Solver.Shared where

import           Control.Monad.RWS                             (gets, modify)

import qualified Saga.Language.Typechecker.Qualification       as Q
import qualified Saga.Language.Typechecker.Solver.Constraints  as C
import           Saga.Language.Typechecker.Solver.Constraints  (Constraint (..),
                                                                Evidence,
                                                                Item (..))
import           Saga.Language.Typechecker.Solver.Monad        (Solution (..),
                                                                SolverEff)

import qualified Effectful.State.Static.Local                  as Eff
import           Saga.Language.Typechecker.Type                (Type)
import qualified Saga.Language.Typechecker.Variables           as Var
import           Saga.Language.Typechecker.Variables           (PolymorphicVar,
                                                                VarType)

import           Effectful                                     (Eff)
import qualified Saga.Language.Typechecker.Inference.Inference as I
import           Saga.Language.Typechecker.Inference.Inference (Tag (..))
import qualified Saga.Language.Typechecker.Kind                as K
import qualified Saga.Language.Typechecker.Type                as T



type family Discriminate a


from :: SolverEff es => Q.Constraint Type -> Eff es Constraint
from (Q.Pure t)          = return $ C.Pure $ Mono t
from (Q.Resource m t)    = return $ C.Resource (Mono t) m
from (Q.Refinement bs re t) = return $ C.Refined (fmap Mono bs) (Mono t) re
from (Q.Implements t p)  = do
    superEv <- fresh E
    return $ C.Impl superEv (Mono t) p
from (Q.Equality t t')   = do
    eqEv <- fresh E
    return $ C.Equality eqEv (Mono t) (Mono t')


fresh :: SolverEff es => Tag a -> Eff es (VarType Type a)
fresh t = do
    Eff.modify $ \s -> s {count = count s + 1}
    index <- Eff.gets count
    let count = show ([1 ..] !! index)
    return $ case t of
        E -> C.Evidence $ "cst_ev_" ++ count
        U -> T.Unification ("cst_uvar_" ++ count) K.Type -- Level 0 = top level

type instance VarType Type I.Evidence       = Var.PolymorphicVar Evidence
type instance VarType Type I.Unification    = Var.PolymorphicVar Type
  --return $ Var.Evidence $ "cst_ev_" ++ count
