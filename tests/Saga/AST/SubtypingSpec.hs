module Saga.AST.SubtypingSpec where

import           Test.Hspec

import qualified Saga.AST.TypeSystem.Inference       as I
import           Saga.AST.TypeSystem.Inference       (Env (kcount, tcount, expressions, typeVars, typeKinds))
import qualified Saga.AST.TypeSystem.Types           as Ty
import qualified Saga.AST.Syntax          as AST

import           Saga.Parser.Parser       as P

import           Data.Bifunctor           (first)
import qualified Data.Map                 as Map

import           Control.Monad.Except
import           Control.Monad.State.Lazy




run :: (Eq a, Show a) => Either String (Ty.Type a) -> Ty.Type a
run inference = case inference of
    Left msg  -> error msg
    Right val -> val

check :: String -> String -> Either String Bool
check ty sub = I.run $ do
  let (Right ty') = P.runSagaType ty
  let (Right sub') = P.runSagaType sub
  ty''  <- I.reduce ty'
  sub'' <- I.reduce sub'
  sub'' `I.isSubtype` ty''

spec :: Spec
spec = do
  describe "Subtyping" $ do
    it "can check primitive subtypes" $ 
      check "String" "String" `shouldBe` Right True

    it "can check polymorphic subtypes" $ 
      check "1" "a" `shouldBe` Right True

    it "can check literal subtypes" $ 
      sequence [check "String" "\"str\"", check "Int" "1", check "Bool" "true"] `shouldBe` Right [True, True, True]

    it "can check tuple subtypes" $
      check "(Int, Bool)" "(Int, Bool, String)" `shouldBe` Right True

    it "can check record subtypes" $ 
      check "{ foo: Int, bar: Bool }" "{ foo: Int, bar: Bool, str: String }" `shouldBe` Right True

    -- it "can check parametric subtypes" $
    --   check "List <Int>" "List <1>" `shouldBe` Right True

    -- it "can check polymorphic parametric subtypes" $
    --   check "List <Int>" "f <a>" `shouldBe` Right True

    it "can check identifier subtypes" $ let
      check = I.run $ do
        let (Right ty) = P.runSagaType "Int"
        let (Right subId) = P.runSagaType "MyTypeId"
        let (Right subTy) = P.runSagaType "1"
        ty'  <- I.reduce ty
        subId' <- I.reduce subId
        subTy' <- I.reduce subTy
        let env = Just $ I.Env { typeVars = Map.fromList [("MyTypeId", subTy')], typeKinds = Map.empty, expressions = Map.empty, tcount = 0, kcount= 0 }
        let (Right val) = I.runInEnv env $ subId' `I.isSubtype` ty'
        return val

      in check `shouldBe` Right True

    describe "can check arrow subtypes" $ do
      it "a -> a is subtype of Int -> Int" $
        check "Int -> Int" "a -> a" `shouldBe` Right True

      it "a -> a is not a subtype of Int -> String" $ 
        check "Int -> String" "a -> a" `shouldBe` Right False



