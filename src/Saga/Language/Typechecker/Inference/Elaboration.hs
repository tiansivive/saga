{-# LANGUAGE TypeFamilies #-}
module Saga.Language.Typechecker.Inference.Elaboration where
import           Saga.Language.Core.Expr             (AST, Expr)
import qualified Saga.Language.Typechecker.Type      as T
import           Saga.Language.Typechecker.Type      (Polymorphic, Type, Typed)
import           Saga.Language.Typechecker.Variables (Classifier)



class Elaborate e where
    elaborate :: (t ~ Classifier e) => e -> Polymorphic t -> e
