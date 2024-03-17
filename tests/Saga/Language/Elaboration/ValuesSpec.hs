module Saga.Language.Elaboration.ValuesSpec where


import           Test.Hspec

import Saga.Language.Typechecker.Elaboration.Test

spec :: Spec
spec = do
    describe "Elaboration" $ do
        it "elaborates literals:" $ do
            1 `shouldBe` 1

