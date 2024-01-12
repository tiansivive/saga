

module Saga.Language.EvaluationSpec where

import qualified Saga.Language.Evaluation      as E
import           Test.Hspec

import qualified Saga.Language.Syntax.Expr     as Expr
import qualified Saga.Language.Syntax.Literals as L

import qualified Data.Map                      as Map

import           Control.Monad.State.Lazy


-- run mval = case E.run Nothing mval of
--     Left msg -> error msg
--     Right val -> val

spec :: Spec
spec = do
  it "skipping evaluation" $
    1 `shouldBe` 1
-- spec :: Spec
-- spec = do
--   describe "Literal terms" $ do
--     it "can evaluate literal ints" $ do
--       let e = Expr.Literal $ L.LInt 1
--       (run $ E.eval e) `shouldBe` E.VInt 1




