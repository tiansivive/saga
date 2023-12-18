


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

import qualified Data.Kind                               as GHC
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
import qualified Saga.Language.Typechecker.Variables     as Var
import           Saga.Language.Typechecker.Variables     (Classifier, VarType,
                                                          Variable)

class
  ( Instantiate (Classifier e)
  , Generalize (Classifier e)
  ) => Inference e where
    -- | Effects required for inference: this should be a tuple in the format `(Effect :> es)`
    type family Effects e (es :: [Eff.Effect]) :: GHC.Constraint
    infer       :: Effects e es                       => e -> Eff es e
    lookup      :: (t ~ Classifier e, Effects e es)   => String -> Eff es (Qualified t)
    fresh       :: (t ~ Classifier e, Effects e es)   => Eff es (Variable t)

    --qualify     :: (t ~ Classifier e, w ~ Constraint t)              => w -> Polymorphic t -> Polymorphic t
class Instantiate t where
    instantiate :: Polymorphic t -> t -> Polymorphic t

-- QUESTION: Is this the right place to define Generalization? It should now happen only after zonking, so there's no need for Inference to depend on it.
class Generalize t where
    -- ENHANCEMENT: Define the needed effects as an associated type
    type family Counter t :: *
    generalize :: (Eff.State Int :> es)  => t -> Eff es (Polymorphic t)




type Instantiable t es = (Eff.Error SagaError :> es, Instantiate t, Show t, Show (Variable t))
instantiateWith :: Instantiable t es => Polymorphic t -> [t] -> Eff es (Polymorphic t)
instantiateWith polymorphic ts = instantiate' polymorphic ts
  where
    instantiate' ty []                 = return ty
    instantiate' ty@(Forall [] qt) ts  = Eff.throwError $ TooManyInstantiationArguments polymorphic ts
    instantiate' ty (t:ts) = do
      let ty' = instantiate ty t
      instantiate' ty' ts

