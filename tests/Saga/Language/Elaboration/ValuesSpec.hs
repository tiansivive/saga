{-# LANGUAGE ViewPatterns #-}

module Saga.Language.Elaboration.ValuesSpec where


import           Test.Hspec

import           Saga.Language.Typechecker.Elaboration.Values.Expressions

import           Data.Either                                              (isLeft)
import qualified Data.Map                                                 as Map
import           Saga.Language.Syntax.AST                                 (AST,
                                                                           Phase (Elaborated))
import qualified Saga.Language.Syntax.Elaborated.AST                      as ES
import qualified Saga.Language.Syntax.Elaborated.Kinds                    as EK
import qualified Saga.Language.Syntax.Elaborated.Types                    as ET
import qualified Saga.Language.Syntax.Elaborated.Values                   as EV

import qualified Saga.Language.Syntax.Reduced.AST                         as RS
import           Saga.Language.Syntax.Reduced.Types                       as RT
import           Saga.Language.Syntax.Reduced.Values                      as RV

import           Saga.Language.Syntax.Literals
import           Saga.Language.Syntax.Polymorphism                        (Given (..),
                                                                           Polymorphic (..),
                                                                           Qualified (..))

import           Saga.Language.Typechecker.Elaboration.Effects            (State (..))
import qualified Saga.Language.Typechecker.Elaboration.Monad              as EM
import           Saga.Language.Typechecker.Env                            (CompilerState (..),
                                                                           Info,
                                                                           Proofs (..))
import           Saga.Language.Typechecker.Errors                         (SagaError (UnboundVariable))
import qualified Saga.Language.Typechecker.Solving.Constraints            as Solver
import qualified Saga.Language.Typechecker.Solving.Shared                 as Solver
import           Saga.Language.Typechecker.Solving.Shared                 (compact)

import           Saga.Utils.Common                                        (fmap2)
import           Saga.Utils.Operators                                     ((|>),
                                                                           (||>))

import           Data.Functor                                             ((<&>))
import           Data.Generics.Biplate                                    (transform)
import           Data.List                                                (find)
import           Debug.Pretty.Simple                                      (pTrace,
                                                                           pTraceM)
import           Saga.Language.Shared

spec :: Spec
spec = do
    describe "Expressions Elaboration" $ do
        let run = EM.run emptyEnv 0 emptyState



        describe "Literal" $ do
            it "annotates integer literals with singleton types" $ do
                int <- run $ RS.Raw (int 1)
                check int $ ast |> \(ES.Annotated expr ty) -> ES.node ty `shouldBe` ET.Singleton (LInt 1)


            it "annotates string literals with singleton types" $ do
                str <- run $ RS.Raw (str "hello")
                check str $ ast |> \(ES.Annotated expr ty) -> ES.node ty `shouldBe` ET.Singleton (LString "hello")

        describe "Functions" $ do

            it "annotates unary functions with Arrow types" $ do
                lam <- run $ RS.Raw (RV.Lambda ["x"] (RS.Raw $ int 1))
                check lam $ ast |> \(ES.Annotated expr (ES.node -> ET.Arrow input output)) -> do
                    ES.node input `shouldBe` ET.Var (ET.Unification "t1" (EK.Var (EK.Unification "k1")))
                    ES.node output `shouldBe` ET.Singleton (LInt 1)

            it "annotates 2-ary functions with Arrow types" $ do
                lam <- run $ RS.Raw (RV.Lambda ["x", "y"] (RS.Raw $ int 1))
                check lam $ ast |> \(ES.Annotated expr (ES.node -> ET.Arrow (ES.node -> arg1) (ES.node -> fn))) -> do
                    let (ET.Arrow (ES.node -> arg2) (ES.node -> output)) = fn

                    arg1 `shouldBe` ET.Var (ET.Unification "t1" (EK.Var (EK.Unification "k1")))
                    arg2 `shouldBe` ET.Var (ET.Unification "t2" (EK.Var (EK.Unification "k2")))
                    output `shouldBe` ET.Singleton (LInt 1)


        describe "Applications" $ do
            let fn = RV.Lambda ["x"] (RS.Raw $ int 1)
            let app = run . RS.Raw $ RV.Application (RS.Raw fn) [RS.Raw $ int 2]
            let fnType = constraint |> compact |> \(Solver.Equality ev (Solver.Ty left) (Solver.Ty right)) -> right
            context "Expression: (\\x -> 1) 2" $ before app $ do

                it "annotates result with a fresh type variable t1" $ \app' -> do
                    check app' $ ast |> \(ES.Annotated expr (ES.node -> ty)) -> do
                        ty `shouldBe`  ET.Var (ET.Unification "t1" (EK.Var (EK.Unification "k1")))

                it "generalizes the function argument type to a polymorphic type" $ \app' -> do
                    check app' $ fnType |> \(ET.Polymorphic (Forall tvars qt)) -> do
                        let (ET.Qualified (_ :=> ET.Arrow (ES.node -> arg) _)) = qt
                        arg `shouldBe` ET.Var (ET.Poly "g3" EK.Type)

                it "qualifies the function argument with the Num protocol" $ \app' -> do
                    check app' $ fnType |> \(ET.Polymorphic (Forall _ qt)) -> do
                        let (ET.Qualified (_ :| cs :=> _)) = qt
                        cs `shouldBe` [ET.Implements (ET.Var $ ET.Poly "g3" EK.Type) "Num"]

                it "emits an equality constraint: t2 -> Singleton 1 :~: forall g3. g3 -> t1" $ \app' -> do
                    check app' $ constraint |> compact |> \(Solver.Equality ev (Solver.Ty left) (Solver.Ty right)) -> do
                        let (ET.Arrow (ES.node -> l_arg) (ES.node -> l_output)) = left
                        let (ET.Polymorphic (Forall tvars qt)) = right
                        let (ET.Qualified (_ :=> ET.Arrow (ES.node -> r_arg) (ES.node -> r_output))) = qt

                        ev `shouldBe` Solver.Evidence "ev1"
                        l_arg `shouldBe` ET.Var (ET.Unification "t2" (EK.Var (EK.Unification "k2")))
                        l_output `shouldBe` ET.Singleton (LInt 1)
                        r_arg `shouldBe` ET.Var (ET.Poly "g3" EK.Type)
                        r_output `shouldBe` ET.Var (ET.Unification "t1" (EK.Var (EK.Unification "k1")))

            context "Field access: foo.bar" $ do
                let access record field = run . RS.Raw $ RV.Application (RS.Raw $ RV.Var ".") [record, RS.Raw $ RV.Var field]
                let record = RV.Record [("bar", RS.Raw (RV.Literal $ LInt 1))]
                let fkv = EK.Var (EK.Unification "k1")
                let ftv = ET.Var (ET.Unification "t1" fkv)

                it "elaborates application into field access constructor" $ do
                    result <- access (RS.Raw record) "bar"
                    check result $ ast |> \(ES.Annotated expr _) -> do
                        expr `shouldSatisfy` (\(EV.FieldAccess {}) -> True)

                it "annotates the record with a record type" $ do
                    result <- access (RS.Raw record) "bar"
                    check result $ ast |> \(ES.Annotated (EV.FieldAccess (ES.extract -> ty) _) _) -> do
                        ty `shouldSatisfy` \(ET.Record [("bar", ES.node -> ET.Singleton (LInt 1))]) -> True

                it "annotates the field access with a fresh type variable" $ do
                    result <- access (RS.Raw record) "bar"
                    check result $ ast |> \(ES.extract -> ty) -> do
                        ty `shouldBe` ftv

                it "emits an equality constraint between the elaborated record type and the expected record type" $ do
                    result <- access (RS.Raw record) "bar"
                    check result $ constraint |> compact |> \(Solver.Equality _ (Solver.Ty left) (Solver.Ty right)) -> do
                        left `shouldBe` ET.Record [("bar", ES.Annotated (ET.Singleton $ LInt 1) (ES.Raw EK.Type))]
                        right `shouldBe` ET.Record [("bar", ES.Annotated ftv (ES.Raw fkv))]

        describe "Match expressions" $ do
            let types = Map.fromList [("Int",  ES.Annotated (ET.Data "Int") (ES.Raw EK.Type))]
            let env = Saga [] Map.empty types Map.empty (Proofs ET.Void Map.empty)

            let match cases = EM.run env 0 emptyState . RS.Raw $
                    RV.Match
                        (RS.Annotated (int 1) (RS.Raw $ RT.Identifier "Int"))
                        (cases <&> \(pat, expr) -> RS.Raw $ RV.Case (RS.Raw pat) (RS.Raw expr))

            it "annotates a match expression with a union type" $ do
                result <- match [(RV.Wildcard, str "test")]
                check result $ ast |> \(ES.Annotated _ (ES.node -> ty)) -> do
                    ty `shouldSatisfy`  (\ET.Union {} -> True)

            it "annotates the resulting union with the case expression types" $ do
                result <- match [(RV.PatLit $ LInt 1, str "test"), (RV.PatLit $ LInt 2, int 10)]
                check result $ ast |> \(ES.Annotated _ (ES.node -> ET.Union tys)) -> do
                    length tys `shouldBe` 2
                    ES.node (tys !! 0) `shouldBe` ET.Singleton (LString "test")
                    ES.node (tys !! 1) `shouldBe` ET.Singleton (LInt 10)


            it "emits an equality constraint between the scrutinee type and a union of all the pattern types" $ do
                result <- match [(RV.PatLit $ LInt 1, str "test"), (RV.PatLit $ LInt 2, int 10)]
                let isTheEqConstraint (Solver.Equality _ (Solver.Ty left) (Solver.Ty right))
                        | ET.Union (fmap ES.node -> tys) <- left
                        , ET.Data "Int" <- right
                        = length tys == 2 && ET.Singleton (LInt 1) `elem` tys && ET.Singleton (LInt 2) `elem` tys
                    isTheEqConstraint _ = False

                check result
                    $ constraint
                    |> Solver.flatten
                    |> (`shouldSatisfy` any isTheEqConstraint)


            it "emits an implication constraint for each case expression" $ do
                result <- match [(RV.PatLit $ LInt 1, str "test"), (RV.PatLit $ LInt 2, int 10)]
                let implication ty (Solver.Implication _ [Solver.Assume eq] _)
                        | Solver.Equality _ (Solver.Ty ty') (Solver.Ty tvar) <- eq
                        ,  ET.Var (ET.Unification {}) <- tvar
                        = ty == ty'
                    implication _ _ = False

                check result $ constraint|> Solver.flatten |> \constraints -> do
                    constraints `shouldSatisfy` any (implication $ ET.Singleton (LInt 1))
                    constraints `shouldSatisfy` any (implication $ ET.Singleton (LInt 2))


        describe "Variables" $ do
            let x = "x"
            let vals = Map.fromList [(x,  ES.Annotated (EV.Literal $ LString x) (ES.Annotated (ET.Data "String") (ES.Raw EK.Type)))]
            let env = Saga [] vals Map.empty Map.empty (Proofs ET.Void Map.empty)

            it "looks up variables in the environment" $ do
                result <- EM.run env 0 emptyState $ RS.Raw (RV.Var x)
                check result $ ast |> \(ES.Annotated expr (ES.node -> ty)) -> do
                    expr `shouldBe` EV.Var (EV.Identifier x)
                    ty `shouldBe` ET.Data "String"

            it "throws UnboundVariable error when encountering undefined variable" $ do
                let y = "y"
                result <- EM.run env 0 emptyState $ RS.Raw (RV.Var y)
                case result of
                    Right (Left (_, UnboundVariable var)) -> var `shouldBe` y
                    _ -> expectationFailure "Expected UnboundVariable error"


        describe "Annotated values" $ do
            let types = Map.fromList [("Int",  ES.Annotated (ET.Data "Int") (ES.Raw EK.Type))]
            let env = Saga [] Map.empty types Map.empty (Proofs ET.Void Map.empty)

            it "emits an equality constraint between the annotated type and the expected type" $ do
                result <- EM.run env 0 emptyState $ RS.Annotated (RV.Literal $ LInt 1) (RS.Raw $ RT.Identifier "Int")
                check result $ constraint |> compact |> \(Solver.Equality _ (Solver.Ty left) (Solver.Ty right)) -> do
                    left `shouldBe` ET.Singleton (LInt 1)
                    right `shouldBe` ET.Data "Int"
            it "rigidifies the annotated type" $ do
                result <- EM.run env 0 emptyState $ RS.Annotated (RV.Literal $ LInt 1) (RS.Raw $ RT.Lambda ["a"] (RT.Identifier "a"))
                check result $ ast |> \(ES.Annotated _ (ES.node -> ty)) -> do
                    ty `shouldBe` ET.Polymorphic
                                    (Forall [ET.Rigid "a" (EK.Var (EK.Unification "k1"))]
                                        (ET.Var (ET.Rigid "a" (EK.Var (EK.Unification "k1"))))
                                    )


int = RV.Literal . LInt
str = RV.Literal . LString

type Result node st = (((AST Elaborated node, Solver.Constraint), st), Info)

ast :: Result node st -> AST Elaborated node
ast (((x, _), _), _) = x
constraint :: Result node st -> Solver.Constraint
constraint (((_, x), _), _) = x
state :: Result node st -> st
state ((_, x), _) = x
info :: Result node st -> Info
info ((_, _), x) = x


