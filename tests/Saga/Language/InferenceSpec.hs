module Saga.Language.InferenceSpec where

import qualified Data.Map                                      as Map
import qualified Data.Set                                      as Set
import qualified Saga.Language.TypeSystem.Inference   


import qualified Saga.Language.Core.Expr as E
import qualified Saga.Language.Core.Literals as L


import           Test.Hspec

import           Control.Monad.State.Lazy
import           Saga.Language.TypeSystem.Environment (Scheme (Scheme))
import           Saga.Language.TypeSystem.Types       
import           Saga.Language.TypeSystem.Inference (simplify)       




-- infer :: String -> Scheme
-- infer input = case I.run input of
--     Left msg  -> error msg
--     Right val -> val

spec :: Spec
spec = do
 
  describe "Inference" $ do
    it "simplifies types:" $ do
      let unit = TVoid
      let pair = [TPrimitive TString, TPrimitive TInt]
      
      let singleton  = TUnion $ Set.fromList [unit]
      let simple = TUnion $ Set.fromList pair
      let repeated = TUnion $ Set.fromList [unit, unit]

      let nested = TUnion $ Set.fromList [unit, simple]
      let sink  = TUnion $ Set.fromList [unit, repeated]

      simplify singleton `shouldBe` unit
      simplify repeated `shouldBe` unit
      simplify sink `shouldBe` unit
      simplify nested `shouldBe` (TUnion $ Set.fromList $ unit : pair)
--     describe "Types:" $ do
--       it "can infer literal types" $ do
--         -- Scheme [] ([] :=> HM.TLiteral (HM.LInt n)) <- return $ infer "1"
--         -- n `shouldBe` 1
--         Scheme [] ([] :=> T.TLiteral (L.LString str)) <- return $ infer "\"string\""
--         str `shouldBe` "string"

--       it "can infer arrow types" $ do
--         Scheme [] ([] :=> T.TArrow (T.TVar input) (T.TVar output)) <- return $ infer "\\x -> x"
--         input `shouldBe` output

--       it "can infer protocol constraints" $ do
--         Scheme [] ([ T.TVar a `T.Implements` "Num"] :=> T.TArrow (T.TVar input) (T.TVar output)) <- return $ infer "\\x -> x + 1"
--         a `shouldBe` input
--         a `shouldBe` output
--         input `shouldBe` output




--     -- describe "Kinds:" $ do
--     --   it "can infer higher kinds" $ do










