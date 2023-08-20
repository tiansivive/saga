module Saga.AST.InferenceSpec where

import qualified Data.Map                                      as Map
import qualified Saga.AST.Syntax                               as AST
import qualified Saga.AST.TypeSystem.HindleyMilner.Inference   as I
import qualified Saga.AST.TypeSystem.HindleyMilner.Types       as HM

import           Test.Hspec

import           Control.Monad.State.Lazy
import           Saga.AST.TypeSystem.HindleyMilner.Environment (Scheme (Scheme))
import           Saga.AST.TypeSystem.HindleyMilner.Types       (Qualified ((:=>)))
import           Saga.Parser.Parser                            as P


infer :: String -> Scheme
infer input = case I.run input of
    Left msg  -> error msg
    Right val -> val

spec :: Spec
spec = do
  describe "Inference" $ do
    describe "Types:" $ do
      it "can infer literal types" $ do
        -- Scheme [] ([] :=> HM.TLiteral (HM.LInt n)) <- return $ infer "1"
        -- n `shouldBe` 1
        Scheme [] ([] :=> HM.TLiteral (HM.LString str)) <- return $ infer "\"string\""
        str `shouldBe` "string"

      it "can infer arrow types" $ do
        Scheme [] ([] :=> HM.TArrow (HM.TVar input) (HM.TVar output)) <- return $ infer "\\x -> x"
        input `shouldBe` output




    -- describe "Kinds:" $ do
    --   it "can infer higher kinds" $ do










