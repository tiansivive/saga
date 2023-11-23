module Saga.Language.Typechecker.Shared where
import           Control.Monad.Except
import qualified Data.Map                            as Map
import           Effectful                           (Eff, (:>))
import qualified Effectful.Error.Static              as Eff
import           Saga.Language.Typechecker.Errors
import           Saga.Language.Typechecker.Kind      (Kind)
import           Saga.Language.Typechecker.Type      (Polymorphic, Tag, Type)
import           Saga.Language.Typechecker.Variables



classifier :: (Eff.Error SagaError :> es, Show t) => PolymorphicVar t -> Eff es (Classifier t)
classifier (Type _ c)           = return c
classifier (Skolem _ c)         = return c
classifier (Unification _ _ c)  = return c
classifier v@(Instantiation {}) = Eff.throwError $ UnexpectedInstantiationVariable v



letters :: [String]
letters = [1 ..] >>= flip replicateM ['α' .. 'ω']
