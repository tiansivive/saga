module Saga.AST.SubtypingSpec where

import qualified Saga.AST.Inference       as I
import qualified Saga.AST.Subtyping       as ST
import qualified Saga.AST.Syntax          as AST
import           Test.Hspec


import           Control.Monad.Except
import           Control.Monad.State.Lazy
import           Data.Bifunctor           (first)
import           Saga.AST.InferenceSpec   as ISpec hiding (run)


run :: Show a => I.Infer a Bool -> Either String Bool
run check = show `first` check'
  where check' = runExcept $ evalStateT check I.initEnv


spec :: Spec
spec = do
  describe "Subtyping" $ do
    it "can detect polymorphic subtypes" $ let
      check = do
        ty1 <- I.infer "a"
        ty2 <- I.infer "1"
        run $ ty1 `ST.isSubtype` ty2

      in check `shouldBe` Right True



