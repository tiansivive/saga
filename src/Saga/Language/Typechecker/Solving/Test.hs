module Saga.Language.Typechecker.Solving.Test where
import           Saga.Language.Syntax.Polymorphism


import qualified Data.Map                                      as Map
import qualified Effectful                                     as Eff
import qualified Effectful.Error.Static                        as Eff
import qualified Effectful.Fail                                as Eff
import qualified Effectful.Reader.Static                       as Eff
import qualified Effectful.State.Static.Local                  as Eff
import qualified Effectful.Writer.Static.Local                 as Eff
import           Saga.Language.Syntax.AST
import           Saga.Language.Syntax.Elaborated.AST
import qualified Saga.Language.Syntax.Elaborated.Kinds         as K
import qualified Saga.Language.Syntax.Elaborated.Types         as T
import           Saga.Language.Syntax.Elaborated.Types         (Variable (..))
import           Saga.Language.Typechecker.Env                 (CompilerState (..),
                                                                Info,
                                                                Proofs (..))
import           Saga.Language.Typechecker.Errors              (SagaError)
import           Saga.Language.Typechecker.Solving.Constraints (Constraint (..),
                                                                Item (..),
                                                                Variable (Evidence))
import           Saga.Language.Typechecker.Solving.Cycles      (Cycle)
import qualified Saga.Language.Typechecker.Solving.Equalities  as Equalities
import           Saga.Language.Typechecker.Solving.Monad       (Count (..),
                                                                Levels,
                                                                initialSolution)
import qualified Saga.Language.Typechecker.Solving.Run         as Solver
import qualified Saga.Language.Typechecker.Variables           as Var
import           Saga.Utils.Operators                          ((|>), (||>))
import           Text.Pretty.Simple                            (pPrint)


initialEnv :: CompilerState Elaborated
initialEnv = Saga [] Map.empty Map.empty Map.empty (Proofs T.Void Map.empty)

-- initialState :: State
-- initialState = IST 0 0 0 Map.empty


run = Eff.runState initialSolution
        |> Eff.runState (Count 0 0)
        |> Eff.runState @[Cycle T.Type] []
        |> Eff.runReader @Levels Map.empty
        |> Eff.runReader (Var.Level 0)
        |> Eff.runReader @(CompilerState Elaborated) initialEnv
        |> Eff.runWriter @Info
        |> Eff.runError @SagaError
        |> Eff.runFail
        |> Eff.runEff


test = Solver.run constraint ||> run >>= pPrint

constraint =
    Equality
        ( Evidence "ev_2" )
        ( Ty
            ( T.Var
                ( Unification "t2"
                    ( K.Var
                        ( K.Unification "k2" )
                    )
                )
            )
        )
        ( Ty
            ( T.Polymorphic
                ( Forall
                    [ Poly "g4" K.Type ]
                    ( T.Qualified
                        ( ( :=> )
                            { given = ( :| )
                                { bindings = Map.empty
                                , constraints =
                                    [ T.Implements
                                        ( T.Var
                                            ( Poly "g4" K.Type )
                                        ) "Num"
                                    ]
                                }
                            , item = T.Arrow
                                ( Annotated
                                    ( T.Var
                                        ( Poly "g4" K.Type )
                                    ) ( Raw K.Type )
                                )
                                ( Annotated
                                    ( T.Var
                                        ( Unification "t3"
                                            ( K.Var
                                                ( K.Unification "k3" )
                                            )
                                        )
                                    )
                                    ( Raw
                                        ( K.Var
                                            ( K.Unification "k3" )
                                        )
                                    )
                                )
                            }
                        )
                    )
                )
            )
        )
