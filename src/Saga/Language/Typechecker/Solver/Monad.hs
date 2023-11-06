{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeFamilies   #-}


module Saga.Language.Typechecker.Solver.Monad where

import           Control.Monad.Except
import           Control.Monad.RWS                             (MonadRWS, gets,
                                                                modify, modify',
                                                                tell)
import           Control.Monad.RWS.Class                       (MonadWriter)
import           Control.Monad.Trans.Writer                    (WriterT)
import qualified Data.Map                                      as Map
import           Saga.Language.Typechecker.Environment
import           Saga.Language.Typechecker.Errors              (SagaError)
import           Saga.Language.Typechecker.Monad               (TypeCheck)
import           Saga.Language.Typechecker.Solver.Constraints  (Constraint,
                                                                Evidence)
import           Saga.Language.Typechecker.Solver.Substitution (Subst,
                                                                Substitutable,
                                                                compose)
import           Saga.Language.Typechecker.Type                (Type)
import qualified Saga.Language.Typechecker.Variables           as Var
import           Saga.Language.Typechecker.Variables           (PolymorphicVar)
import qualified Effectful.State.Static.Local as Eff


type SolverEff es = TypeCheck (Eff.State State : es)
type SolverM = SolverEff '[]

data State = St { count :: Int, evidence :: Subst Evidence, tvars :: Subst Type }

class Solve c where
    solve       :: c -> SolverM Constraint
    simplify    :: c -> SolverM Constraint

    irreducible :: c -> Bool


emit' :: Tag a -> Subst (Of a) -> SolverM ()
emit' E sub = Eff.modify $ \s -> s{ evidence = sub `Map.union` evidence s }
emit' T sub = Eff.modify $ \s -> s{ tvars = sub `compose` tvars s }

data Ev = Ev
data Ty = Ty
data Tag a where
  E  :: Tag Ev
  T  :: Tag Ty
type family Of a where
    Of Ev = Evidence
    Of Ty = Type
