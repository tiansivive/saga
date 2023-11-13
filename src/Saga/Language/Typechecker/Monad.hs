{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
module Saga.Language.Typechecker.Monad where
import           Control.Monad.Except
import           Control.Monad.RWS
import           Effectful                             (Eff)
import           Effectful.Error.Static                (Error)
import qualified Effectful.Error.Static                as Eff
import           Effectful.Fail                        (Fail)
import qualified Effectful.Fail                        as Eff
import           Effectful.Reader.Static               (Reader)
import qualified Effectful.Reader.Static               as Eff
import           Effectful.Writer.Static.Local         (Writer)
import qualified Effectful.Writer.Static.Local         as Eff
import           Saga.Language.Typechecker.Environment hiding (Error)
import           Saga.Language.Typechecker.Errors      (SagaError)
import           Saga.Language.Typechecker.Lib         (defaultEnv)


-- type TypeCheck s m  = (MonadRWS CompilerState Info s m, MonadError SagaError m, MonadFail m)

-- class (MonadReader CompilerState m, MonadWriter Info m, MonadError SagaError m) => TypeCheck m where
--     inform :: Info -> m ()

type TypeCheck es = Eff (Reader CompilerState ': Writer Info ': Error SagaError ': Fail ': es)

run :: TypeCheck es a -> Eff es (Either String (Either (Eff.CallStack, SagaError) (a, Info)))
run = Eff.runFail . Eff.runError . Eff.runWriter . Eff.runReader defaultEnv
