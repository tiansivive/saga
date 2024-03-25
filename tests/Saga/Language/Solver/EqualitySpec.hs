module Saga.Language.Solver.EqualitySpec where

import qualified Data.Map                                      as Map
import qualified Saga.Language.Syntax.Elaborated.Types         as ET
import           Saga.Language.Typechecker.Env                 (CompilerState (..),
                                                                Info,
                                                                Proofs (Proofs))
import qualified Saga.Language.Typechecker.Solving.Constraints as Solver

import qualified Saga.Language.Typechecker.Solving.Monad       as SM
import           Test.Hspec

import           Saga.Language.Shared
import qualified Saga.Language.Syntax.Elaborated.Kinds         as EK
import           Saga.Language.Syntax.Elaborated.Types         (Type)
import           Saga.Language.Typechecker.Solving.Constraints (Constraint)
import           Saga.Language.Typechecker.Solving.Cycles      (Cycle)
import qualified Saga.Language.Typechecker.Solving.Equalities  as EQ
import           Saga.Language.Typechecker.Solving.Monad       (Count,
                                                                Solution (tvars),
                                                                Status)
import           Saga.Language.Typechecker.Solving.Run
import           Saga.Utils.Operators                          ((|>), (||>))

spec :: Spec
spec = do

    describe "Constraint Solving" $ do
        let env = Saga [] Map.empty Map.empty Map.empty (Proofs ET.Void Map.empty)

        context "Equalities" $ do


            it "should solve when the types are equal" $ do
                let ty = ET.Data "Int"
                    eq = Solver.Equality (Solver.Evidence "ev") (Solver.Ty ty) (Solver.Ty ty)
                result <- SM.run' env $ EQ.solve eq
                check result $ constraint |> \(status, c) -> do
                    status `shouldBe` SM.Solved
                    c `shouldBe` Solver.Empty

            it "should emit a substitution solution when equaling a type var to a type" $ do
                let tvar = ET.Poly "t" EK.Type
                    ty = ET.Data "Int"
                    eq = Solver.Equality (Solver.Evidence "ev") (Solver.Ty $ ET.Var tvar) (Solver.Ty ty)
                result <- SM.run' env $ EQ.solve eq
                check result $ \res -> do

                    res ||> constraint |> \(status, c) -> do
                        status `shouldBe` SM.Solved
                        c `shouldBe` Solver.Empty
                    res ||> solution |> \(SM.Solution { tvars }) -> do
                        tvars `shouldBe` Map.singleton tvar ty


type Result = (((((Status, Constraint), Solution), Count), [Cycle Type]), Info)

constraint :: Result -> (Status, Constraint)
constraint ((((x, _), _), _), _) = x

solution :: Result -> Solution
solution ((((_, x), _), _), _) = x

