{-# LANGUAGE ViewPatterns #-}
module Saga.Language.Elaboration.TypesSpec where



import qualified Data.Map                                          as Map
import           Saga.Language.Syntax.AST                          (AST,
                                                                    Phase (Elaborated))
import qualified Saga.Language.Syntax.Elaborated.AST               as ES
import qualified Saga.Language.Syntax.Elaborated.Types             as ET

import qualified Saga.Language.Syntax.Elaborated.Kinds             as EK
import           Saga.Language.Syntax.Literals                     (Literal (..))
import qualified Saga.Language.Syntax.Reduced.AST                  as RS
import qualified Saga.Language.Syntax.Reduced.Types                as RT
import           Saga.Language.Typechecker.Elaboration.Effects     (State (..))
import qualified Saga.Language.Typechecker.Elaboration.Monad       as EM
import           Saga.Language.Typechecker.Elaboration.Types.Types
import           Saga.Language.Typechecker.Env                     (CompilerState (..),
                                                                    Info,
                                                                    Proofs (..))
import qualified Saga.Language.Typechecker.Solving.Constraints     as Solver
import           Saga.Utils.Operators                              ((|>))
import           Test.Hspec



spec :: Spec
spec = do
    describe "Type Elaboration" $ do

        context "Literal" $ do
            let run = EM.run emptyEnv 0 emptyState
            it "annotates singleton types with kind Type" $ do
                result <- run $ RS.Raw (RT.Singleton $ LInt 1)
                check result $ ast |> \(ES.Annotated _ (ES.node -> k)) -> do
                    k `shouldBe` EK.Type

        context "Functions" $ do
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



check result action = case result of
    Left x -> expectationFailure $ "Expected Right value.\nGot: " ++ show x
    Right (Left err) -> expectationFailure $ "Expected Right value.\nGot: " ++ show err
    Right (Right res) -> action res



type Result node st = (((AST Elaborated node, Solver.Constraint), st), Info)

ast :: Result node st -> AST Elaborated node
ast (((x, _), _), _) = x
constraint :: Result node st -> Solver.Constraint
constraint (((_, x), _), _) = x
state :: Result node st -> st
state ((_, x), _) = x
info :: Result node st -> Info
info ((_, _), x) = x

emptyEnv :: CompilerState Elaborated
emptyEnv = Saga [] Map.empty Map.empty Map.empty (Proofs ET.Void Map.empty)

emptyState :: State
emptyState = IST 0 0 0 Map.empty
