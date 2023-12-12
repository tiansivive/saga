{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}


module Saga.Language.Typechecker.Solver.Monad where

import           Control.Monad.Except
import           Control.Monad.RWS                             (MonadRWS, gets,
                                                                modify, modify',
                                                                tell)
import           Control.Monad.RWS.Class                       (MonadWriter)
import           Control.Monad.Trans.Writer                    (WriterT)
import qualified Data.Map                                      as Map
import qualified Effectful.State.Static.Local                  as Eff
import           Saga.Language.Typechecker.Environment
import           Saga.Language.Typechecker.Errors              (SagaError)
import           Saga.Language.Typechecker.Monad               (TypeCheck)
import           Saga.Language.Typechecker.Solver.Constraints  (Constraint,
                                                                Evidence,
                                                                Variable,
                                                                Witnessed)
import           Saga.Language.Typechecker.Solver.Substitution (Subst,
                                                                Substitutable,
                                                                compose)

import           Debug.Pretty.Simple                           (pTraceM)
import           Effectful                                     (Eff, (:>))
import qualified Effectful                                     as Eff
import           GHC.Stack.Types                               (CallStack)
import qualified Saga.Language.Typechecker.Monad               as TC
import           Saga.Language.Typechecker.Solver.Cycles       (Cycle)
import           Saga.Language.Typechecker.Type                (Type)



type SolverEff es = (TypeCheck es, Eff.State Solution :> es, Eff.State [Cycle Type] :> es)


data Solution = Solution { count :: Int, evidence :: Subst Evidence, tvars :: Subst Type, witnessed :: Witnessed }
  deriving (Show)
data Status = Solved | Deferred | Impossible deriving Show

class Solve c where
    solve       :: SolverEff es => c -> Eff es (Status, Constraint)
    simplify    :: SolverEff es => c -> Eff es Constraint

    irreducible :: c -> Bool
    irreducible = const True


initialSolution :: Solution
initialSolution = Solution { count = 0, evidence = Map.empty, tvars = Map.empty, witnessed = Map.empty }



update :: SolverEff es => Tag a -> Subst (Of a) -> Eff es ()
update E sub = Eff.modify $ \s -> s{ evidence = sub `Map.union` evidence s }
update T sub = Eff.modify $ \s -> s{ tvars = sub `compose` tvars s }

data Ev = Ev
data Ty = Ty
data Tag a where
  E  :: Tag Ev
  T  :: Tag Ty

type family Of a where
    Of Ev = Evidence
    Of Ty = Type




run :: Eff (Eff.State [Cycle Type] : Eff.State Solution : es) a -> Eff es ((a, [Cycle Type]), Solution)
run =  Eff.runState initialSolution . Eff.runState []
