


{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}


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
import           Saga.Language.Typechecker.Variables     (Classifier,
                                                          PolymorphicVar,
                                                          VarType)



type InferEff es w = TypeCheck (Eff.State State ': Eff.Writer w ': es)
type InferM w = InferEff '[] w

data State = IST
  { vars  :: Int
  , level :: Int
  } deriving (Show)

class
  ( Instantiate (Classifier e)
  , Generalize (Classifier e)
  ) => Inference e where
    infer       :: (t ~ Classifier e, w ~ EmittedConstraint t)  => e -> InferM w e
    lookup      :: (t ~ Classifier e, w ~ EmittedConstraint t)  => String -> InferM w (Qualified t)
    fresh       :: (t ~ Classifier e, w ~ EmittedConstraint t)  => Tag a -> InferM w (VarType e a)

    --qualify     :: (t ~ Classifier e, w ~ Constraint t)              => w -> Polymorphic t -> Polymorphic t

class Instantiate t where
    instantiate :: Polymorphic t -> t -> Polymorphic t
class Generalize t where
    generalize :: w ~ EmittedConstraint t => t -> InferM w (Polymorphic t)

type family EmittedConstraint t :: *

data Tag a where
  E   :: Tag Evidence
  Sk  :: Tag Skolem
  U   :: Tag Unification
  T   :: Tag TypeVar
  I   :: Tag Instantiation

data Evidence = Evidence
data Unification = Unification
data Instantiation = Instantiation
data TypeVar = TypeVar
data Skolem = Skolem


inform' :: Info -> InferEff es w ()
inform' = Eff.tell

type Instantiable t es = (Eff.Error SagaError :> es, Instantiate t, Show t, Show (PolymorphicVar t))
instantiateWith :: Instantiable t es => Polymorphic t -> [t] -> Eff es (Polymorphic t)
instantiateWith polymorphic ts = instantiate' polymorphic ts
  where
    instantiate' ty []                 = return ty
    instantiate' ty@(Forall [] qt) ts  = Eff.throwError $ TooManyInstantiationArguments polymorphic ts
    instantiate' ty (t:ts) = do
      let ty' = instantiate ty t
      instantiate' ty' ts


initialState :: State
initialState = IST 0 0


run :: Monoid w => InferEff es w a -> Eff es ((Either String (Either (Eff.CallStack, SagaError) (a, Info)), State), w)
run = Eff.runWriter . Eff.runState initialState . TC.run
