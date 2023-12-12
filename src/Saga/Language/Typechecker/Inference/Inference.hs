


{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}


module Saga.Language.Typechecker.Inference.Inference where


import           Control.Monad.RWS
import           Control.Monad.Trans.Writer              (WriterT (runWriterT),
                                                          runWriter)
import           Data.Data                               (Proxy)
import qualified Data.Map                                as Map
import           Saga.Language.Typechecker.Environment
import           Saga.Language.Typechecker.Errors        (SagaError (TooManyArguments, TooManyInstantiationArguments))
import           Saga.Language.Typechecker.Kind          (Kind)

import           Effectful                               (Eff, (:>))
import qualified Effectful                               as Eff
import qualified Effectful.Error.Static                  as Eff
import qualified Effectful.Fail                          as Eff
import qualified Effectful.Reader.Static                 as Eff
import qualified Effectful.State.Static.Local            as Eff
import qualified Effectful.Writer.Static.Local           as Eff
import qualified Saga.Language.Typechecker.Monad         as TC
import           Saga.Language.Typechecker.Monad         (TypeCheck)
import           Saga.Language.Typechecker.Qualification (Qualified)
import           Saga.Language.Typechecker.Type          (Polymorphic,
                                                          Scheme (..), Type)
import           Saga.Language.Typechecker.Variables     (Classifier, VarType,
                                                          Variable)

class
  ( Instantiate (Classifier e)
  , Generalize (Classifier e)
  ) => Inference e where
    -- ENHANCEMENT: Define the needed effects as an associated type
    type family State e :: *
    infer       :: (s ~ State e, t ~ Classifier e, w ~ EmittedConstraint t, InferEff es s w)  => e -> Eff es e
    lookup      :: (s ~ State e, t ~ Classifier e, w ~ EmittedConstraint t, InferEff es s w)  => String -> Eff es (Qualified t)
    fresh       :: (s ~ State e, t ~ Classifier e, w ~ EmittedConstraint t, InferEff es s w)  => Eff es (Variable t)

    --qualify     :: (t ~ Classifier e, w ~ Constraint t)              => w -> Polymorphic t -> Polymorphic t
class Instantiate t where
    instantiate :: Polymorphic t -> t -> Polymorphic t
class Generalize t where
    -- ENHANCEMENT: Define the needed effects as an associated type
    type family St t :: *
    generalize :: (Eff.State (St t) :> es)  => t -> Eff es (Polymorphic t)

type family EmittedConstraint t :: *


type InferEff es s w = (TypeCheck es, Eff.State s :> es, Eff.Writer w :> es, Monoid w)


type Instantiable t es = (Eff.Error SagaError :> es, Instantiate t, Show t, Show (Variable t))
instantiateWith :: Instantiable t es => Polymorphic t -> [t] -> Eff es (Polymorphic t)
instantiateWith polymorphic ts = instantiate' polymorphic ts
  where
    instantiate' ty []                 = return ty
    instantiate' ty@(Forall [] qt) ts  = Eff.throwError $ TooManyInstantiationArguments polymorphic ts
    instantiate' ty (t:ts) = do
      let ty' = instantiate ty t
      instantiate' ty' ts

