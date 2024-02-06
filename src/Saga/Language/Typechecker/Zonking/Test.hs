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
import qualified Saga.Language.Syntax.Protocols                as P
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
initialEnv = Saga [protocol] Map.empty Map.empty Map.empty (Proofs ZT.Void Map.empty)


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
    let ((zonked, qualified), info) = res
    putStrLn "\n--------------------- INFO --------------------- "
    pPrint info
    putStrLn "\n--------------------- ZONKED --------------------- "
    pPrint zonked
    putStrLn "\n--------------------- QUALIFIED TYPE --------------------- "
    pPrint qualified

    return ()



ast = Z.Annotated
    ( Z.Var $ Z.Identifier "f")
    ( Z.Annotated
        ( ZT.Arrow
            ( Z.Annotated
                ( ZT.Var $ ZT.Poly "t1" ZK.Type )
                ( Z.Raw ZK.Type )
            )
            ( Z.Annotated
                ( ZT.Var $ ZT.Poly "t2" ( ZK.Var $ ZK.Poly "k1" ))
                ( Z.Raw $ ZK.Var ( ZK.Poly "k1" ))
            )
        )
        ( Z.Raw ZK.Type ))




sol = S.Solution
    { evidence = mempty
    , tvars = Map.fromList
        [
            ( ZT.Poly "t1" ZK.Type
            , ZT.Arrow
                ( Z.Annotated
                    ( ZT.Var $ ZT.Poly "t3" ZK.Type )
                    ( Z.Raw ZK.Type )
                )
                ( Z.Annotated
                    ( ZT.Data "Int" )
                    ( Z.Raw ZK.Type )
                )
            )
        ,
            ( ZT.Poly "t2" ( ZK.Var $ ZK.Poly "k1" )
            , ZT.Data "Int"
            )
        ]
    , kvars = Map.fromList
        [ ( ZK.Poly "k1", ZK.Type )
        ]
    , witnessed = mempty
    , proofs = mempty
    }


residuals = [ Constraint.Implementation
    ( Constraint.Evidence "ev" )
    ( ZT.Var $ ZT.Poly "t3" ZK.Type)
    "Show"
    ]



protocol =
  P.Protocol
    "Show"
    ( Forall [var] (ZT.Record
        [ ("show", Z.Raw $ tvar `ZT.Arrow` Z.Raw (ZT.Data "String"))
        ])
    )
    []
    where
      t = "z"
      var = ZT.Poly t ZK.Type
      tvar = Z.Raw $ ZT.Var var
