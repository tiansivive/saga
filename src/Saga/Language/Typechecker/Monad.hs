{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
module Saga.Language.Typechecker.Monad where
import           Control.Monad.Except
import           Control.Monad.RWS
import           Effectful                             (Eff, IOE, (:>))
import qualified Effectful                             as Eff
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



type TypeCheck es = (IOE :> es, Reader CompilerState :> es, Writer Info :> es,  Error SagaError :> es, Fail :> es)


run :: (TypeCheck es, Monoid w) => Eff es a -> Eff es (Either String (Either (Eff.CallStack, e) (a, w)))
run = Eff.runFail . Eff.runError . Eff.runWriter . Eff.runReader defaultEnv . Eff.inject
