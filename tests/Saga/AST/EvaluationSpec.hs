

module Saga.AST.EvaluationSpec where

import Test.Hspec
import qualified Saga.AST.Evaluation as E
import qualified Saga.AST.TypeSystem.HindleyMilner.Types as HM

import qualified Data.Map as Map

import   Control.Monad.State.Lazy


run mval = case E.run Nothing mval of
    Left msg -> error msg
    Right val -> val


spec :: Spec
spec = do
  describe "Literal terms" $ do
    it "can evaluate literal ints" $ do
      let e = HM.Term $ HM.LInt 1
      (run $ E.eval e) `shouldBe` E.VInt 1




