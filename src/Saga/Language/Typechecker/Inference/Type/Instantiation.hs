module Saga.Language.Typechecker.Inference.Type.Instantiation where
import qualified Data.Map                                      as Map
import           Saga.Language.Typechecker.Inference.Inference
import           Saga.Language.Typechecker.Solver.Substitution (Substitutable (..))
import           Saga.Language.Typechecker.Type                (Scheme (..),
                                                                Type)





instance Instantiate Type  where
  instantiate qt@(Forall [] _) t = return qt
  instantiate (Forall (tvar:tvars) qt) t = return $ Forall tvars qt'
    where
      sub = Map.fromList [(tvar, t)]
      qt' = apply sub qt

