{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}
module Saga.Language.Elaboration.TypesSpec where



import qualified Data.Map                                               as Map
import           Saga.Language.Syntax.AST                               (AST,
                                                                         Phase (Elaborated))
import qualified Saga.Language.Syntax.Elaborated.AST                    as ES
import qualified Saga.Language.Syntax.Elaborated.Types                  as ET

import           Debug.Pretty.Simple                                    (pTrace,
                                                                         pTraceM)
import           Documentation.SBV.Examples.WeakestPreconditions.IntDiv (DivS (q))
import           Saga.Language.Shared
import qualified Saga.Language.Syntax.Elaborated.Kinds                  as EK
import           Saga.Language.Syntax.Literals                          (Literal (..))
import           Saga.Language.Syntax.Polymorphism                      (Polymorphic (..))
import qualified Saga.Language.Syntax.Reduced.AST                       as RS
import qualified Saga.Language.Syntax.Reduced.Kinds                     as RK
import qualified Saga.Language.Syntax.Reduced.Types                     as RT
import           Saga.Language.Typechecker.Elaboration.Effects          (State (..))
import qualified Saga.Language.Typechecker.Elaboration.Monad            as EM
import           Saga.Language.Typechecker.Elaboration.Types.Types
import           Saga.Language.Typechecker.Env                          (CompilerState (..),
                                                                         Info,
                                                                         Proofs (..))
import qualified Saga.Language.Typechecker.Solving.Constraints          as Solver
import           Saga.Language.Typechecker.Solving.Shared               (flatten)
import           Saga.Utils.Operators                                   ((|>))
import           Test.Hspec



spec :: Spec
spec = do
    describe "Type Elaboration" $ do

        describe "Literals" $ do
            let run = EM.run emptyEnv 0 emptyState
            it "annotates singleton types with kind Type" $ do
                result <- run $ RS.Raw (RT.Singleton $ LInt 1)
                check result $ ast |> \(ES.Annotated _ (ES.node -> k)) -> do
                    k `shouldBe` EK.Type

        describe "Functions" $ do
            let fn = RS.Raw (RT.Arrow (RT.Identifier "Int") (RT.Identifier "Int"))
            let types = Map.fromList [("Int",  ES.Annotated (ET.Data "Int") (ES.Raw EK.Type))]
            let env = Saga [] Map.empty types Map.empty (Proofs ET.Void Map.empty)
            let run = EM.run env 0 emptyState

            it "annotates arrow types with kind Type" $ do
                result <- run fn
                check result $ ast |> \(ES.Annotated ty k) -> ES.node k `shouldBe` EK.Type

            it "annotates both arguments and return type with kind Type" $ do
                result <- run fn
                check result $ ast |> \(ES.Annotated (ET.Arrow input output) k) -> do
                    input `shouldBe` ES.Annotated (ET.Data "Int") (ES.Raw EK.Type)
                    output `shouldBe` ES.Annotated (ET.Data "Int") (ES.Raw EK.Type)

        describe "Lambdas" $ do
            let lam = RS.Raw (RT.Lambda ["x"] (RT.Identifier "x"))
            let env = Saga [] Map.empty Map.empty Map.empty (Proofs ET.Void Map.empty)
            let run = EM.run env 0 emptyState

            it "elaborates type lambdas as polymorphic types" $ do
                result <- run lam
                check result $ ast |> \(ES.Annotated ty _) -> do
                    ty `shouldSatisfy` \case
                        ET.Polymorphic {} -> True
                        _ -> False

            it "annotates type lambdas with Arrow kinds" $ do
                result <- run lam
                check result $ ast |> \(ES.Annotated _ (ES.node -> k)) -> do
                    k `shouldBe` EK.Arrow (ES.Raw (EK.Var (EK.Unification "k1"))) (ES.Raw (EK.Var (EK.Unification "k1")))

        describe "Applications" $ do

            context "expression: F Int! where F :: Type -> Type = forall t. t -> Int" $ do
                let int = ES.Annotated (ET.Data "Int") (ES.Raw EK.Type)
                let tvar = ET.Poly "t" EK.Kind
                let arg = ES.Annotated (ET.Var tvar) (ES.Raw EK.Kind)
                let poly = ET.Polymorphic $ Forall [tvar] (ET.Arrow arg int)
                let app = RS.Raw (RT.Application (RT.Identifier "F") [RT.Identifier "Int"])
                let f = ES.Annotated poly (ES.Raw $ EK.Arrow (ES.Raw EK.Type) (ES.Raw EK.Type))
                let types = Map.fromList
                                [ ("Int",  int)
                                , ("F",  f)
                                ]
                let env = Saga [] Map.empty types Map.empty (Proofs ET.Void Map.empty)
                let run = EM.run env 0 emptyState

                it "substitutes the current type with a fresh type variable" $ do
                    result <- run app
                    check result $ ast |> \(ES.Annotated ty _) -> do
                        ty `shouldBe` ET.Var (ET.Unification "t1" $ EK.Var (EK.Unification "k1"))

                it "annotates type applications with a fresh kind variable" $ do
                    result <- run app
                    check result $ ast |> \(ES.Annotated ty k) -> do
                        ES.node k `shouldBe` EK.Var (EK.Unification "k1")

                it "emits an equality constraint between the inferred F kind and the elaborated F kind" $ do
                    result <- run app
                    check result $ constraint |> flatten |> \cs -> do
                        cs `shouldSatisfy` any (\case
                            Solver.Equality _
                                (Solver.K (EK.Arrow (ES.Raw EK.Type) (ES.Raw EK.Type)))
                                (Solver.K (EK.Polymorphic (Forall [] arr)))
                                | EK.Arrow (ES.Raw EK.Type) (ES.Raw k) <- arr
                                , EK.Var (EK.Unification "k1") <- k
                                -> True
                            _ -> False)

                it "emits an evaluation constraint for the applied type" $ do
                    result <- run app
                    check result $ constraint |> flatten |> \cs -> do
                        let sub = ES.Annotated (ET.Var (ET.Unification "t1" $ EK.Var (EK.Unification "k1"))) (ES.Raw $ EK.Var (EK.Unification "k1"))
                        cs `shouldSatisfy` any (\case
                            Solver.Evaluate result ty -> result == sub && ty == ET.Applied f int
                            _ -> False)

            context "Record field access: foo.bar" $ do
                let record = RT.Record [("bar", RT.Identifier "Int")]
                let app = RS.Raw (RT.Application (RT.Identifier ".") [record, RT.Identifier "bar"])

                let types = Map.fromList [("Int",  ES.Annotated (ET.Data "Int") (ES.Raw EK.Type))]
                let env = Saga [] Map.empty types Map.empty (Proofs ET.Void Map.empty)
                let run = EM.run env 0 emptyState

                it "annotates record field access with the type of the field" $ do
                    result <- run app
                    check result $ ast |> \(ES.Annotated ty _) -> do
                        ty `shouldBe` ET.Data "Int"

        describe "Pattern Matching" $ do
            let match = RS.Raw (RT.Match (RT.Identifier "x") [RT.Case (RS.Raw RT.Wildcard) (RS.Raw $ RT.Identifier "String")])
            let types = Map.fromList
                            [ ("x",  ES.Annotated (ET.Data "Int") (ES.Raw EK.Type))
                            , ("String",  ES.Annotated (ET.Data "String") (ES.Raw EK.Type))
                            ]
            let env = Saga [] Map.empty types Map.empty (Proofs ET.Void Map.empty)
            let fkv = EK.Var (EK.Unification "k1")
            let sub = ET.Var (ET.Unification "t1" fkv)
            let run = EM.run env 0 emptyState

            it "annotates match expressions with a fresh kind variable" $ do
                result <- run match
                check result $ ast |> \(ES.Annotated _ k) -> do
                    ES.node k `shouldBe` fkv

            it "substitutes the current type with a fresh type variable" $ do
                result <- run match
                check result $ ast |> \(ES.Annotated ty _) -> do
                    ty `shouldBe` sub

            it "emits an evaluation constraint for the matched type" $ do
                result <- run match
                check result $ constraint |> flatten |> \cs -> do
                    let sub' = ES.Annotated sub (ES.Raw fkv)
                    let int = ES.Annotated (ET.Data "Int") (ES.Raw EK.Type)
                    let str = ES.Annotated (ET.Data "String") (ES.Raw EK.Type)
                    let case' = ET.Case (ES.Raw ET.Wildcard) str
                    cs `shouldSatisfy` any (\case
                        Solver.Evaluate result ty -> result == sub' && ty == ET.Match int [ES.Raw case']
                        _ -> False)

        describe "Annotated types - NOT YET IMPLEMENTED" $ do
            let types = Map.fromList [("Int",  ES.Annotated (ET.Data "Int") (ES.Raw EK.Type))]
            let env = Saga [] Map.empty types Map.empty (Proofs ET.Void Map.empty)

            return ()



type Result node st = (((AST Elaborated node, Solver.Constraint), st), Info)

ast :: Result node st -> AST Elaborated node
ast (((x, _), _), _) = x
constraint :: Result node st -> Solver.Constraint
constraint (((_, x), _), _) = x
state :: Result node st -> st
state ((_, x), _) = x
info :: Result node st -> Info
info ((_, _), x) = x

