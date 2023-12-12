{-# LANGUAGE TypeFamilies #-}
module Saga.Language.Typechecker.Solver.Shared where

import           Control.Monad.RWS                             (gets, modify)

import qualified Saga.Language.Typechecker.Qualification       as Q
import qualified Saga.Language.Typechecker.Solver.Constraints  as C
import           Saga.Language.Typechecker.Solver.Constraints  (Constraint (..),
                                                                Evidence,
                                                                Item (..))
import           Saga.Language.Typechecker.Solver.Monad        (Count (evs, tvs),
                                                                Solution (..),
                                                                SolverEff)

import qualified Effectful.State.Static.Local                  as Eff
import           Saga.Language.Typechecker.Type                (Type)
import qualified Saga.Language.Typechecker.Variables           as Var
import           Saga.Language.Typechecker.Variables           (VarType,
                                                                Variable)

import           Effectful                                     (Eff)
import qualified Saga.Language.Typechecker.Inference.Inference as I

import qualified Saga.Language.Typechecker.Kind                as K
import qualified Saga.Language.Typechecker.Type                as T
import           Saga.Utils.Operators                          ((|>))




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




data Tag a where
    E :: Tag Evidence
    T :: Tag Type

fresh :: SolverEff es => Tag a -> Eff es (Variable a)
fresh E = do
    i <- Eff.gets $ evs |> (+1)
    Eff.modify $ \s -> s { evs = i}
    let count = show ([1 ..] !! i)
    return $ C.Evidence $ "ev_" ++ count
fresh T = do
    i <- Eff.gets $ tvs |> (+1)
    Eff.modify $ \s -> s {tvs  = i}
    let count = show ([1 ..] !! i)
    return $ T.Poly ("ct_" ++ count) K.Type
    -- return $ case t of
    --     E -> C.Evidence $ "cst_ev_" ++ count
    --     U -> T.Unification ("cst_uvar_" ++ count) K.Type -- Level 0 = top level

