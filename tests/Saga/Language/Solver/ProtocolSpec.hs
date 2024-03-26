{-# LANGUAGE LambdaCase #-}
module Saga.Language.Solver.ProtocolSpec where


import qualified Data.Map                                      as Map
import           Debug.Pretty.Simple                           (pTraceM)
import           Saga.Language.Shared
import           Saga.Language.Syntax.AST                      (Phase (Elaborated))
import qualified Saga.Language.Syntax.Elaborated.Kinds         as EK
import qualified Saga.Language.Syntax.Elaborated.Types         as ET
import           Saga.Language.Syntax.Elaborated.Types         (Type)
import qualified Saga.Language.Syntax.Elaborated.Values        as EV
import           Saga.Language.Syntax.Literals
import           Saga.Language.Syntax.Polymorphism             (Given ((:|)),
                                                                Polymorphic (..),
                                                                Qualified ((:=>)))
import qualified Saga.Language.Syntax.Protocols                as P
import           Saga.Language.Syntax.Protocols                (Protocol (..))
import           Saga.Language.Typechecker.Env                 (CompilerState (..),
                                                                Info,
                                                                Proofs (..))
import           Saga.Language.Typechecker.Errors              (SagaError (MissingProtocolImplementation))
import qualified Saga.Language.Typechecker.Solving.Constraints as Solver
import           Saga.Language.Typechecker.Solving.Constraints (Constraint)
import           Saga.Language.Typechecker.Solving.Cycles      (Cycle)
import qualified Saga.Language.Typechecker.Solving.Monad       as SM
import           Saga.Language.Typechecker.Solving.Monad       (Count, Solution (..),
                                                                Status)
import qualified Saga.Language.Typechecker.Solving.Protocols   as P
import           Saga.Utils.Operators                          ((|>), (||>))
import           Test.Hspec


spec :: Spec
spec = do
    describe "Solving protocol implementation constraints" $ do
        let env = Saga [protocol] Map.empty Map.empty Map.empty (Proofs ET.Void Map.empty)

        it "throws missing protocol implementation error" $ do
            let impl = Solver.Implementation (Solver.Evidence "ev") (ET.Data "string") "protocol"
            result <- SM.run' env $ P.solve impl
            result `shouldSatisfy` \case
                Right (Left (_, MissingProtocolImplementation {})) -> True
                _ -> False

        it "updates evidence with the implementation value" $ do
            let impl = Solver.Implementation (Solver.Evidence "ev") (ET.Data "Int") "protocol"
            result <- SM.run' env $ P.solve impl
            check result $ solution |>SM.evidence |> \witness -> do
                witness `shouldBe` Map.singleton (Solver.Evidence "ev") (Solver.Protocol implValueTest)

        it "should propagate the protocol implementation constraints" $ do
            pendingWith "Need to implement protocol search by unification constraint propagation"

        it "should entail the constraint if it find existing evidence" $ do
            pendingWith "Need to add a runner with the initial solution as a parameter"

        

type Result = (((((Status, Constraint), Solution), Count), [Cycle Type]), Info)


constraint ((((x, _), _), _), _) = x
solution ((((_, x), _), _), _) = x



protocol :: Protocol Elaborated
protocol =
  Protocol
    "protocol"
    ( Forall [] (ET.Data "Int")
    )
    [ implValueTest ]


implValueTest = P.Implementation (P.Name "test", ET.Data "Int", EV.Literal $ LInt 1)
