{-# LANGUAGE LambdaCase #-}
module Saga.Language.Solver.UnificationSpec where


import qualified Data.Map                                      as Map
import           Debug.Pretty.Simple                           (pTrace, pTraceM)
import           Saga.Language.Shared
import qualified Saga.Language.Syntax.Elaborated.AST           as ES
import qualified Saga.Language.Syntax.Elaborated.Kinds         as EK
import qualified Saga.Language.Syntax.Elaborated.Types         as ET
import           Saga.Language.Syntax.Literals                 (Literal (LInt))
import           Saga.Language.Typechecker.Env                 (CompilerState (Saga),
                                                                Info,
                                                                Proofs (..))
import           Saga.Language.Typechecker.Errors              (SagaError (..))
import qualified Saga.Language.Typechecker.Solving.Constraints as Solver
import           Saga.Language.Typechecker.Solving.Constraints (Constraint)
import           Saga.Language.Typechecker.Solving.Cycles      (Cycle)
import           Saga.Language.Typechecker.Solving.Monad       (Count, Solution)
import           Saga.Language.Typechecker.Solving.Shared      (compact,
                                                                flatten)
import qualified Saga.Language.Typechecker.Solving.Unification as U
import           Saga.Language.Typechecker.Substitution        (Subst)
import           Saga.Utils.Operators                          ((|>))
import           Test.Hspec


spec :: Spec
spec = do
    let env = Saga [] Map.empty Map.empty Map.empty (Proofs ET.Void Map.empty)
    let annInt = ES.Annotated (ET.Data "Int") (ES.Raw EK.Type)
    let annStr = ES.Annotated (ET.Data "String") (ES.Raw EK.Type)

    let tyCon = ES.Annotated (ET.Data "Con") (ES.Raw $ EK.Arrow (ES.Raw EK.Type) (ES.Raw EK.Type))




    describe "Unification" $ do

        it "Int ~ Int" $ do
            result <- U.run env $ U.unify (ET.Data "Int") (ET.Data "Int")
            check result $ subst |> (`shouldBe` Map.empty)

        it "Int ~! String" $ do
            result <- U.run env $ U.unify (ET.Data "Int") (ET.Data "String")
            result `shouldSatisfy` \case
                Right (Left (_, UnificationFail {})) -> True
                _      -> False

        it "Int ~ Var.Poly a" $ do
            let tvar = ET.Poly "a" EK.Type
            result <- U.run env $ U.unify (ET.Var tvar) (ET.Data "Int")
            check result $ subst |> (`shouldBe` Map.singleton tvar (ET.Data "Int"))

            result <- U.run env $ U.unify (ET.Data "Int") (ET.Var tvar)
            check result $ subst |> (`shouldBe` Map.singleton tvar (ET.Data "Int"))

        it "Var.Poly a ~ Var.Poly b" $ do
            let tvar1 = ET.Poly "a" EK.Type
                tvar2 = ET.Poly "b" EK.Type
            result <- U.run env $ U.unify (ET.Var tvar1) (ET.Var tvar2)
            check result $ subst |> (`shouldBe` Map.singleton tvar1 (ET.Var tvar2))

            result <- U.run env $ U.unify (ET.Var tvar2) (ET.Var tvar1)
            check result $ subst |> (`shouldBe` Map.singleton tvar2 (ET.Var tvar1))

        it "Int -> Int ~ Int -> Int" $ do
            let arr = ET.Arrow annInt annInt
            result <- U.run env $ U.unify arr arr
            check result $ \res -> do
                subst res `shouldBe` Map.empty
                (flatten . compact . constraint) res `shouldSatisfy` \cs ->
                    length cs == 2 && all (\(Solver.Equality _ (Solver.K k) (Solver.K k1)) -> k == k1 && k == EK.Type) cs

        it "Con Int ~ Con Int" $ do
            let conInt = ET.Applied tyCon annInt
            result <- U.run env $ U.unify conInt conInt
            check result $ subst |> (`shouldBe` Map.empty)
            check result $ \res -> do
                subst res `shouldBe` Map.empty
                (flatten . compact . constraint) res `shouldSatisfy` \cs -> length cs == 2
                    && any (\(Solver.Equality _ (Solver.K k) (Solver.K k1)) -> k == k1 && k == EK.Type) cs
                    && any (\(Solver.Equality _ (Solver.K k) (Solver.K k1)) -> k == k1 && k == EK.Arrow (ES.Raw EK.Type) (ES.Raw EK.Type)) cs

        it "Singleton 1 ~ Singleton 1" $ do
            let singleton = ET.Singleton $ LInt 1
            result <- U.run env $ U.unify singleton singleton
            check result $ subst |> (`shouldBe` Map.empty)
        it "Singleton 1 ~! Singleton 2" $ do
            let singleton1 = ET.Singleton $ LInt 1
                singleton2 = ET.Singleton $ LInt 2
            result <- U.run env $ U.unify singleton1 singleton2
            result `shouldSatisfy` \case
                Right (Left (_, UnificationFail {})) -> True
                _      -> False

        describe "Subtyping" $ do

            it "Singleton 1 <: Int" $ do
                let singleton = ET.Singleton $ LInt 1
                result <- U.run env $ U.unify singleton (ET.Data "Int")
                check result $ subst |> (`shouldBe` Map.empty)
            it "Int <: Singleton 1" $ do
                pendingWith "Subtyping failure is not implemented yet"

            it "Int ~ Int | String" $ do
                let union = ET.Union [annInt, annStr]
                result <- U.run env $ U.unify (ET.Data "Int") union
                check result $ subst |> (`shouldBe` Map.empty)
            it "Int | String ~! Int" $ do
                let union = ET.Union [annInt, annStr]
                result <- U.run env $ U.unify union (ET.Data "Int")
                result `shouldSatisfy` \case
                    Right (Left (_, UnificationFail {})) -> True
                    _      -> False



type Result t = (((((((Subst t, [Cycle t]), Proofs), Constraint), Solution), Count), [Cycle t]), Info)

subst (((((((s, _), _), _), _), _), _), _) = s


constraint (((((((_, _), _), c), _), _), _), _) = c
