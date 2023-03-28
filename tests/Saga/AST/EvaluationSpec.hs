

module Saga.AST.EvaluationSpec where

import Test.Hspec
import qualified Saga.AST.Evaluation as E
import qualified Saga.AST.Syntax as AST
import qualified Data.Map as Map

import   Control.Monad.State.Lazy

run :: (Eq a, Show a) => E.EvalState a -> E.Value a
run mval = case evalStateT mval Map.empty of
    Left msg -> error msg
    Right val -> val


spec :: Spec
spec = do
  describe "Literal terms" $ do
    it "can evaluate literal ints" $ do
      let ast = AST.Term $ AST.LInt () 1
      (run $ E.eval ast) `shouldBe` (E.VInt 1 :: E.Value ())




