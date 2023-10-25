{-# LANGUAGE TypeFamilies #-}
module Saga.Language.Typechecker.Inference.Elaboration where
import           Saga.Language.Typechecker.Type      (Polymorphic)
import           Saga.Language.Typechecker.Variables (Classifier)


class Elaborate e where
    elaborate :: (t ~ Classifier e) => e -> Polymorphic t -> e
