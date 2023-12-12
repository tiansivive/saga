module Saga.Language.Typechecker.Shared where
import           Control.Monad.Except
import qualified Data.Map                            as Map
import           Effectful                           (Eff, (:>))
import qualified Effectful.Error.Static              as Eff
import           Saga.Language.Typechecker.Errors
import           Saga.Language.Typechecker.Kind      (Kind)
import qualified Saga.Language.Typechecker.Type      as T
import           Saga.Language.Typechecker.Type      (Polymorphic, Tag, Type)
import           Saga.Language.Typechecker.Variables


classifier :: (Eff.Error SagaError :> es) => Variable Type -> Eff es (Classifier Type)
classifier (T.Poly _ c)           = return c
classifier (T.Existential _ c)    = return c
classifier (T.Unification _ c)    = return c
classifier v@(T.Instantiation {}) = Eff.throwError $ UnexpectedInstantiationVariable v

letters :: [String]
letters = [1 ..] >>= flip replicateM ['α' .. 'ω']
