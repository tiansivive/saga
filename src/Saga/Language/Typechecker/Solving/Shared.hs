{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Typechecker.Solving.Shared where
import           Control.Monad                                 (foldM)
import qualified Data.Map                                      as Map
import           Effectful                                     (Eff)
import qualified Effectful.State.Static.Local                  as Eff
import qualified Saga.Language.Syntax.Elaborated.Kinds         as K
import qualified Saga.Language.Syntax.Elaborated.Types         as T
import           Saga.Language.Syntax.Elaborated.Types         (Type)
import qualified Saga.Language.Typechecker.Solving.Constraints as Solver
import           Saga.Language.Typechecker.Solving.Constraints (Evidence)
import           Saga.Language.Typechecker.Solving.Monad       (Count (..),
                                                                Solution (..),
                                                                Solving,
                                                                Witness)
import           Saga.Language.Typechecker.Substitution        (Subst, compose)
import           Saga.Language.Typechecker.Variables           (Variable)
import           Saga.Utils.Operators                          ((|>))





flatten :: Solver.Constraint -> [Solver.Constraint]
flatten (Solver.Conjunction left right) = flatten left ++ flatten right
flatten c                               = pure c


merge :: [Solver.Constraint] -> Solver.Constraint
merge = foldl Solver.Conjunction Solver.Empty


propagate :: Solving es => [T.TypeConstraint] -> Eff es Solver.Constraint
propagate = foldM (\acc c -> Solver.Conjunction acc <$> from c) Solver.Empty

from :: Solving es => T.TypeConstraint -> Eff es Solver.Constraint
--from (T.Pure t)          = return $ C.Pure $ Mono t
--from (Q.Resource m t)    = return $ C.Resource (Mono t) m
from (T.Refinement bs re t) = return $ Solver.Refinement bs t re
from (T.Implements t p)  = do
    superEv <- fresh E
    return $ Solver.Implementation superEv t p
-- from (Q.Equality t t')   = do
--     eqEv <- fresh E
--     return $ C.Equality eqEv (Mono t) (Mono t')




update :: Solving es => Tag a -> Sub a -> Eff es ()
update E sub = Eff.modify $ \s -> s{ evidence = sub `Map.union` evidence s }
update T sub = Eff.modify $ \s -> s{ tvars = sub `compose` tvars s }



fresh :: Solving es => Tag a -> Eff es (Var a)
fresh E = do
    i <- Eff.gets $ evs |> (+1)
    Eff.modify $ \s -> s { evs = i}
    let count = show ([1 ..] !! i)
    return $ Solver.Evidence $ "ev" ++ count
fresh T = do
    i <- Eff.gets $ tvs |> (+1)
    Eff.modify $ \s -> s {tvs  = i}
    let count = show ([1 ..] !! i)
    kvar <- K.Var <$> fresh K
    return $ T.Unification ("soT" ++ count) kvar
fresh K = do
    i <- Eff.gets $ kvs |> (+1)
    Eff.modify $ \s -> s {kvs  = i}
    let count = show ([1 ..] !! i)
    return $ K.Unification ("soK" ++ count)


data Tag a where
  E  :: Tag Evidence
  T  :: Tag T.Type
  K  :: Tag K.Kind



type family Var a where
    Var T.Type  = Variable T.Type
    Var K.Kind  = Variable K.Kind
    Var Evidence = Variable Solver.Constraint

type family Sub a where
    Sub Type     = Subst Type
    Sub Evidence = Witness

