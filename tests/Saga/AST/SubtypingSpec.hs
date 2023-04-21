module Saga.AST.SubtypingSpec where

import           Test.Hspec

import qualified Saga.AST.TypeSystem.Inference       as I
import           Saga.AST.TypeSystem.Inference       (Env (count, expressions, typeVars))
import qualified Saga.AST.TypeSystem.Subtyping       as ST
import qualified Saga.AST.TypeSystem.Types           as Ty
import qualified Saga.AST.Syntax          as AST

import           Saga.Parser.Parser       as P

import           Data.Bifunctor           (first)
import qualified Data.Map                 as Map

import           Control.Monad.Except
import           Control.Monad.State.Lazy






spec :: Spec
spec = do
  describe "Subtyping" $ do
    it "can check primitive subtypes" $ let
      check = do
        ty  <- I.reduce <$> P.runSagaType "String"
        sub <- I.reduce <$> P.runSagaType "String"
        I.run $ sub `ST.isSubtype` ty

      in check `shouldBe` Right True

    it "can check polymorphic subtypes" $ let
      check = do
        ty  <- I.reduce <$> P.runSagaType "1"
        sub <- I.reduce <$> P.runSagaType "a"
        I.run $ sub `ST.isSubtype` ty

      in check `shouldBe` Right True

    it "can check literal subtypes" $ let
      checkStr = do
        ty  <- I.reduce <$> P.runSagaType "String"
        sub <- I.reduce <$> P.runSagaType "\"str\""
        I.run $ sub `ST.isSubtype` ty
      checkInt = do
        ty  <- I.reduce <$> P.runSagaType "Int"
        sub <- I.reduce <$> P.runSagaType "1"
        I.run $ sub `ST.isSubtype` ty
      checkBool = do
        ty  <- I.reduce <$> P.runSagaType "Bool"
        sub <- I.reduce <$> P.runSagaType "true"
        I.run $ sub `ST.isSubtype` ty

      in sequence [checkStr, checkInt, checkBool] `shouldBe` Right [True, True, True]

    it "can check tuple subtypes" $ let
      check = do
        ty  <- I.reduce <$> P.runSagaType "(Int, Bool)"
        sub <- I.reduce <$> P.runSagaType "(Int, Bool, String)"
        I.run $ sub `ST.isSubtype` ty

      in check `shouldBe` Right True

    it "can check record subtypes" $ let
      check = do
        ty  <- I.reduce <$> P.runSagaType "{ foo: Int, bar: Bool }"
        sub <- I.reduce <$> P.runSagaType "{ foo: Int, bar: Bool, str: String }"
        I.run $ sub `ST.isSubtype` ty

      in check `shouldBe` Right True

    it "can check parametric subtypes" $ let
      check = do
        ty  <- I.reduce <$> P.runSagaType "List <Int>"
        sub <- I.reduce <$> P.runSagaType "List <1>"
        I.run $ sub `ST.isSubtype` ty

      in check `shouldBe` Right True

    it "can check polymorphic parametric subtypes" $ let
      check = do
        ty  <- I.reduce <$> P.runSagaType "List <Int>"
        sub <- I.reduce <$> P.runSagaType "f <a>"
        I.run $ sub `ST.isSubtype` ty

      in check `shouldBe` Right True

    it "can check identifier subtypes" $ let
      check = do
        ty    <- I.reduce <$> P.runSagaType "Int"
        subId <- I.reduce <$> P.runSagaType "MyTypeId"
        subTy <- I.reduce <$> P.runSagaType "1"
        let env = Just $ I.Env { typeVars = Map.fromList [("MyTypeId", subTy)], expressions = Map.empty, count = 0 }
        I.runInEnv env $ subId `ST.isSubtype` ty

      in check `shouldBe` Right True

    describe "can check arrow subtypes" $ do
      it "a -> a is subtype of Int -> Int" $ let
        check = do
          ty  <- I.reduce <$> P.runSagaType "Int -> Int"
          sub <- I.reduce <$> P.runSagaType "a -> a"
          I.run $ sub `ST.isSubtype` ty

        in check `shouldBe` Right True

      it "a -> a is not a subtype of Int -> String" $ let
        check = do
          ty  <- I.reduce <$> P.runSagaType "Int -> String"
          sub <- I.reduce <$> P.runSagaType "a -> a"
          I.run $ sub `ST.isSubtype` ty

        in check `shouldBe` Right False



