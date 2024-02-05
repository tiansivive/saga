module Saga.Language.Typechecker.Zonking.Test where

import qualified Data.Map                                      as Map
import qualified Data.Set                                      as Set
import qualified Saga.Language.Syntax.Elaborated.AST           as Z
import qualified Saga.Language.Syntax.Elaborated.Kinds         as ZK
import qualified Saga.Language.Syntax.Elaborated.Types         as ZT
import qualified Saga.Language.Syntax.Elaborated.Values        as Z
import           Saga.Language.Syntax.Polymorphism             (Polymorphic (..))
import qualified Saga.Language.Typechecker.Shared              as Shared

import qualified Effectful                                     as Eff
import qualified Effectful.Error.Static                        as Eff
import qualified Effectful.Fail                                as Eff
import qualified Effectful.Reader.Static                       as Eff
import qualified Effectful.Writer.Static.Local                 as Eff
import           Prelude                                       hiding (print)
import           Saga.Language.Syntax.AST                      (Phase (Elaborated))
import           Saga.Language.Syntax.Literals                 (Literal (LString))
import           Saga.Language.Typechecker.Env                 (CompilerState (..),
                                                                Info,
                                                                Proofs (..))
import           Saga.Language.Typechecker.Errors              (SagaError)
import qualified Saga.Language.Typechecker.Lib                 as Lib
import qualified Saga.Language.Typechecker.Solving.Constraints as Constraint
import qualified Saga.Language.Typechecker.Solving.Monad       as S
import           Saga.Language.Typechecker.Solving.Monad       (Solution (..))
import           Saga.Language.Typechecker.Zonking.Monad       (Context (Context))
import qualified Saga.Language.Typechecker.Zonking.Run         as Run
import           Saga.Utils.Operators                          ((|>), (||>))
import           Text.Pretty.Simple                            (pPrint)



initialEnv :: CompilerState Elaborated
initialEnv = Saga [Lib.numProtocol] Map.empty Map.empty Map.empty (Proofs ZT.Void Map.empty)


test = Run.run ast ||> run >>= print

run = Eff.runReader (Context sol residuals)
        |> Eff.runReader @(CompilerState Elaborated) initialEnv
        |> Eff.runWriter @Info
        |> Eff.runError @SagaError
        |> Eff.runFail
        |> Eff.runEff

print (Left fail) = do
    putStrLn "FAILURE:"
    pPrint fail
    return ()
print (Right (Left (callstack, err))) = do
    putStrLn "\n---------------------\nSAGA ERROR:\n"
    pPrint err
    putStrLn "\n---------------------\nCallstack:\n"
    pPrint callstack
    return ()
print (Right (Right res)) = do
    let (zonked, info) = res
    putStrLn "\n--------------------- INFO --------------------- "
    pPrint info
    putStrLn "\n--------------------- ZONKED --------------------- "
    pPrint zonked

    return ()



ast = Z.Annotated
    (Z.Literal $ LString "Hello, World!" )
    (Z.Raw $ ZT.Polymorphic
                ( Forall
                    [ ZT.Poly "g4" ZK.Type ]
                    (  ZT.Arrow
                                ( Z.Annotated
                                    ( ZT.Var
                                        ( ZT.Poly "g4" ZK.Type )
                                    ) ( Z.Raw ZK.Type )
                                )
                                ( Z.Annotated
                                    ( ZT.Var
                                        ( ZT.Poly "t3"
                                            ( ZK.Var
                                                ( ZK.Poly "k3" )
                                            )
                                        )
                                    )
                                    ( Z.Raw
                                        ( ZK.Var
                                            ( ZK.Poly "k3" )
                                        )
                                    )
                                )

                        )
                    ))



sol = S.Solution
    { evidence = mempty
    , tvars = Map.fromList
        [
            ( ZT.Poly "g4" ZK.Type
            , ZT.Var
                ( ZT.Unification "soT2"
                    ( ZK.Var
                        ( ZK.Unification "soK2" )
                    )
                )
            )
        ,
            ( ZT.Unification "t2"
                ( ZK.Var
                    ( ZK.Unification "k2" )
                )
            , ZT.Arrow
                ( Z.Annotated
                    ( ZT.Var
                        ( ZT.Unification "soT2"
                            ( ZK.Var
                                ( ZK.Unification "soK2" )
                            )
                        )
                    ) ( Z.Raw ZK.Type )
                )
                ( Z.Annotated
                    ( ZT.Var
                        ( ZT.Unification "t3"
                            ( ZK.Var
                                ( ZK.Unification "k3" )
                            )
                        )
                    )
                    ( Z.Raw
                        ( ZK.Var
                            ( ZK.Unification "k3" )
                        )
                    )
                )
            )
        ]
    , kvars = mempty
    , witnessed = mempty
    , proofs = mempty
    }

residuals = [ Constraint.Implementation
    ( Constraint.Evidence "ev3" )
    ( ZT.Var
        ( ZT.Unification "soT2"
            ( ZK.Var
                ( ZK.Unification "soK2" )
            )
        )
    ) "Num"
    ]
