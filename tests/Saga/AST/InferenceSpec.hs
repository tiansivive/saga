module Saga.AST.InferenceSpec where

import qualified Data.Map                      as Map
import qualified Saga.AST.Syntax               as AST
import qualified Saga.AST.TypeSystem.Inference as I
import qualified Saga.AST.TypeSystem.Types     as Ty
import           Test.Hspec

import           Control.Monad.State.Lazy
import           Saga.Parser.Parser            as P


run :: (Eq a, Show a) => Either String (Ty.Type a) -> Ty.Type a
run inference = case inference of
    Left msg  -> error msg
    Right val -> val

spec :: Spec
spec = do
  describe "Inference" $ do
    describe "Types:" $ do
      it "can infer literal types" $ do
        (Ty.TLiteral (AST.LInt info n)) <- return $ run $ I.infer "1"
        n `shouldBe` 1
        (Ty.TLiteral (AST.LString info str)) <- return $ run $ I.infer "\"string\""
        str `shouldBe` "string"
      it "can infer arrow types" $ do
        (Ty.TArrow info arg body) <- return $ run $ I.infer "\\n -> n"
        let Ty.Type (Ty.TVar (AST.Name _ a1)) = arg
        let (Ty.Type (Ty.TVar (AST.Name _ a2))) = body
        a1 `shouldBe` "ta"
        a2 `shouldBe` "ta"


    -- describe "Kinds:" $ do
    --   it "can infer higher kinds" $ do










