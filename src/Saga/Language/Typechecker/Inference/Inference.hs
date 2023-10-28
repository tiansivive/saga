


{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}


module Saga.Language.Typechecker.Inference.Inference where

import           Control.Monad.Except
import           Control.Monad.RWS
import           Control.Monad.Trans.Writer              (WriterT (runWriterT),
                                                          runWriter)
import           Data.Data                               (Proxy)
import qualified Data.Map                                as Map
import           Saga.Language.Typechecker.Environment
import           Saga.Language.Typechecker.Errors        (SagaError (TooManyArguments, TooManyInstantiationArguments))
import           Saga.Language.Typechecker.Kind          (Kind)

import           Saga.Language.Typechecker.Qualification (Qualified)
import           Saga.Language.Typechecker.Type          (Polymorphic,
                                                          Scheme (..), Type)
import           Saga.Language.Typechecker.Variables     (Classifier,
                                                          PolymorphicVar,
                                                          VarType)



type InferM w m  = (MonadRWS CompilerState (Either Info [w]) State m, MonadError SagaError m, MonadFail m)


data State = IST
  { vars        :: Int
  , level       :: Int
  , unification :: Map.Map String (PolymorphicVar Type)
  } deriving (Show)


class
  ( Instantiate (Classifier e)
  , Generalize (Classifier e)
  ) => Inference e where
    infer       :: (t ~ Classifier e, w ~ EmittedConstraint t, InferM w m)  => e -> m e
    lookup      :: (t ~ Classifier e, w ~ EmittedConstraint t, InferM w m)  => String -> m (Qualified t)
    fresh       :: (t ~ Classifier e, w ~ EmittedConstraint t, InferM w m)  => Tag a -> m (VarType e a)

    --qualify     :: (t ~ Classifier e, w ~ Constraint t)              => w -> Polymorphic t -> Polymorphic t

    emit        :: (t ~ Classifier e, w ~ EmittedConstraint t, InferM w m) => w -> m ()
    inform      :: (t ~ Classifier e, w ~ EmittedConstraint t, InferM w m) => Info -> m ()
    emit = emit'
    inform = inform'


class Instantiate t where
    instantiate :: (w ~ EmittedConstraint t, InferM w m) => Polymorphic t -> t -> m (Polymorphic t)

class Generalize t where
    generalize ::  (w ~ EmittedConstraint t, InferM w m) => t -> m (Polymorphic t)


emit' :: InferM w m => w -> m ()
emit' = tell .  Right . pure

inform' :: InferM w m => Info -> m ()
inform' = tell . Left

instantiateWith :: (Show t, Instantiate t, w ~ EmittedConstraint t, InferM w m) => Polymorphic t -> [t] -> m (Polymorphic t)
instantiateWith polymorphic ts = instantiate' polymorphic ts
  where
    instantiate' ty []                 = return ty
    instantiate' ty@(Forall [] qt) ts = throwError $ TooManyInstantiationArguments polymorphic ts
    instantiate' ty (t:ts) = do
      ty' <- instantiate ty t
      instantiate' ty' ts


type family EmittedConstraint t :: *

data Tag a where
  E   :: Tag Evidence
  Sk  :: Tag Skolem
  U   :: Tag Unification
  P   :: Tag PolyVar
  I   :: Tag Instantiation

data Evidence = Evidence
data Unification = Unification
data Instantiation = Instantiation
data PolyVar = PolyVar
data Skolem = Skolem
