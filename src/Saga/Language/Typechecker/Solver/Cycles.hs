module Saga.Language.Typechecker.Solver.Cycles where
import Saga.Language.Typechecker.Variables (PolymorphicVar)
import Saga.Language.Typechecker.Solver.Substitution (Subst, compose)
import Effectful (Eff, (:>))
import qualified Effectful.Error.Static as Eff
import Saga.Language.Typechecker.Errors (SagaError (..))
import Saga.Language.Typechecker.Type (Type)
import qualified Data.Map as Map
import qualified Saga.Language.Typechecker.Type as T

type Cycle t = (PolymorphicVar t, t, Subst t)

collapse :: Eff.Error SagaError :> es => Subst Type -> Cycle Type -> Eff es (Subst Type)
collapse sub (tvar, ty, solution) =
      if unbound tvar sub then
        return $ compose solution sub
      else Eff.throwError $ InfiniteType tvar ty


-- | TODO: Check if any of the tvars are constrained. If so, then try to unify within the union. If it fails, it's an actual error
unbound :: PolymorphicVar Type -> Subst Type -> Bool
unbound tvar subst = case Map.lookup tvar subst of
  Just (T.Var tvar) -> unbound tvar subst
  Just _           -> False
  Nothing          -> True
