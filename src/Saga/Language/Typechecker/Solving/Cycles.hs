module Saga.Language.Typechecker.Solving.Cycles where


import qualified Data.Map                               as Map
import           Effectful                              (Eff, (:>))
import qualified Effectful.Error.Static                 as Eff
import           Saga.Language.Typechecker.Errors       (SagaError (..))
import           Saga.Language.Typechecker.Substitution (Subst, compose)

import           Saga.Language.Syntax.Elaborated.Types  (Type)
import qualified Saga.Language.Syntax.Reduced.Types     as T

import           Saga.Language.Typechecker.Traversals
import           Saga.Language.Typechecker.Variables    (Variable)

type Cycle t = (Variable t, t, Subst t)

-- | TODO: Check type errors in commented code
collapse :: Eff.Error SagaError :> es => Subst Type -> Cycle Type -> Eff es (Subst Type)
collapse sub (tvar, ty, solution) = undefined
      -- if unbound tvar sub then
      --   return $ compose solution sub
      -- else Eff.throwError $ InfiniteType tvar ty


-- | TODO: Check if any of the tvars are constrained. If so, then try to unify within the union. If it fails, it's an actual error
unbound :: Variable Type -> Subst Type -> Bool
unbound tvar subst = case Map.lookup tvar subst of
  -- Just (T.Var tvar) -> unbound tvar subst
  Just _  -> False
  Nothing -> True
