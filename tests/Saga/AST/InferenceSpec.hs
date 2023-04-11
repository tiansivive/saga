module Saga.AST.InferenceSpec where

import qualified Data.Map                 as Map
import qualified Saga.AST.Inference       as I
import qualified Saga.AST.Syntax          as AST
import           Test.Hspec

import           Control.Monad.State.Lazy

run :: (Eq a, Show a) => Either String (AST.Type a) -> AST.Type a
run inference = case inference of
    Left msg  -> error msg
    Right val -> val

spec :: Spec
spec = do
  describe "Inference" $ do
    it "can infer literal types" $ do
        (AST.TLiteral (AST.LInt info n)) <- return $ run $ I.infer "1"
        n `shouldBe` 1



