module Saga.Language.Typechecker.Shared where
import           Control.Monad.Except
import           Saga.Language.Typechecker.Errors
import           Saga.Language.Typechecker.Variables


classifier :: (MonadError SagaError m, Show t) => PolymorphicVar t -> m (Classifier t)
classifier (PolyVar _ c)        = return c
classifier (Skolem _ c)         = return c
classifier (Unification _ _ c)  = return c
classifier v@(Instantiation {}) = throwError $ UnexpectedInstantiationVariable v

