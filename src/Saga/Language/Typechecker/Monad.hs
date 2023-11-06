{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
module Saga.Language.Typechecker.Monad where
import           Control.Monad.Except
import           Control.Monad.RWS
import           Effectful                             (Eff)
import           Effectful.Error.Static                (Error)
import           Effectful.Fail                        (Fail)
import           Effectful.Reader.Static               (Reader)
import           Effectful.Writer.Static.Local         (Writer)
import           Saga.Language.Typechecker.Environment hiding (Error)
import           Saga.Language.Typechecker.Errors      (SagaError)


-- type TypeCheck s m  = (MonadRWS CompilerState Info s m, MonadError SagaError m, MonadFail m)

-- class (MonadReader CompilerState m, MonadWriter Info m, MonadError SagaError m) => TypeCheck m where
--     inform :: Info -> m ()

type TypeCheck es = Eff (Reader CompilerState ': Writer Info ': Error SagaError ': Fail ': es)
