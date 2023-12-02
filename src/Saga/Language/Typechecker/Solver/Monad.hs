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
                                                                PolymorphicVar,
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



-- | FIXME: #23 @tiansivive Effects: Use member constraints
type SolverEff es = (TypeCheck es, Eff.State Solution :> es, Eff.State [Cycle Type] :> es)
type SolverM a = forall es.  (SolverEff es) => Eff es a

data Solution = Solution { count :: Int, evidence :: Subst Evidence, tvars :: Subst Type, witnessed :: Witnessed }
  deriving (Show)
data Status = Solved | Deferred | Impossible deriving Show

class Solve c where
    solve       :: c -> SolverM (Status, Constraint)
    simplify    :: c -> SolverM Constraint

    irreducible :: c -> Bool
    irreducible = const True


initialSolution :: Solution
initialSolution = Solution { count = 0, evidence = Map.empty, tvars = Map.empty, witnessed = Map.empty }



update :: Tag a -> Subst (Of a) -> SolverM ()
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


