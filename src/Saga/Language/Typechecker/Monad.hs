{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
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
import           Saga.Utils.TypeLevel                  (type (§))



type TypeCheck es = (IOE :> es, Reader CompilerState :> es, Writer Info :> es,  Error SagaError :> es, Fail :> es)



run :: Eff '[Reader CompilerState, Writer Info, Error SagaError, Fail, IOE] a -> IO § Either String (Either (Eff.CallStack, SagaError) (a, Info))
run s = Eff.runEff . Eff.runFail . Eff.runError . Eff.runWriter $ Eff.runReader defaultEnv s
