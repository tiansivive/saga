{-# LANGUAGE ViewPatterns #-}
module Saga.Language.Typechecker.Solving.Evaluation where

import           Effectful                                         (Eff, (:>))
import qualified Saga.Language.Syntax.Elaborated.AST               as AST
import qualified Saga.Language.Syntax.Elaborated.Kinds             as K
import qualified Saga.Language.Syntax.Elaborated.Types             as T

import qualified Effectful.Reader.Static                           as Eff
import qualified Effectful.State.Static.Local                      as Eff
import qualified Effectful.Writer.Static.Local                     as Eff
import qualified Saga.Language.Syntax.AST                          as NT (NodeType (..))
import           Saga.Language.Syntax.AST                          (Phase (Elaborated))
import           Saga.Language.Syntax.Elaborated.AST               (node)
import qualified Saga.Language.Syntax.Reduced.AST                  as RD
import           Saga.Language.Typechecker.Elaboration.Effects     (State (IST))
import           Saga.Language.Typechecker.Elaboration.Monad       (Elaboration (..),
                                                                    Instantiate (..))
import qualified Saga.Language.Typechecker.Env                     as Env
import           Saga.Language.Typechecker.Env                     (CompilerState)
import           Saga.Language.Typechecker.Errors                  (Exception (Unexpected),
                                                                    crash)
import qualified Saga.Language.Typechecker.Solving.Constraints     as Solver
import           Saga.Language.Typechecker.Solving.Monad           (Levels,
                                                                    Solving,
                                                                    Status (..))
import qualified Saga.Language.Typechecker.Solving.Shared          as Shared
import           Saga.Language.Typechecker.Solving.Shared          (Tag (..))
import           Saga.Utils.Operators                              ((|>))


import           Saga.Language.Typechecker.Elaboration.Types.Types



solve :: forall es. Solving es => Solver.Constraint -> Eff es (Status, Solver.Constraint)
solve c@(Solver.Evaluate result ty) = case ty of
    T.Applied cons (node -> (T.Var v)) -> return (Deferred, c)

    T.Applied cons arg -> do
        (ty, constraint) <- Eff.runWriter @Solver.Constraint $ reduce cons arg
        ev <- Shared.fresh E
        let eq = Solver.Equality ev (Solver.K $ AST.extract result) (Solver.K $ AST.extract ty)
        let conjunction = foldl Solver.Conjunction (Solver.Evaluate result $ node ty) [eq, constraint]

        return (Solved, conjunction)


    T.Closure [] tyExpr scope -> do
        (ty', constraint) <- run $ elaborate (RD.Raw tyExpr)
        return (Solved, Solver.Conjunction constraint $ Solver.Evaluate result (node ty'))

        where
            run action = do
                levels <- Eff.ask @Levels
                env <- Eff.ask @(CompilerState Elaborated)
                Eff.runWriter @Solver.Constraint
                    |> Eff.evalState (IST 0 0 0 levels)
                    |> Eff.runReader @(CompilerState Elaborated) (env { Env.types = T.types scope, Env.kinds = T.kinds scope })
                    $ action


    _ -> return (Solved, c)




solve c = crash $ Unexpected c "for Evaluate constraint"


simplify ::Solving es => Solver.Constraint -> Eff es Solver.Constraint
simplify c@(Solver.Evaluate result ty) = case ty of
    T.Applied {} -> return c
    T.Closure {} -> return c

    _ -> do
        ev <- Shared.fresh E
        return (Solver.Equality ev (Solver.Ty $ node result) (Solver.Ty ty))



reduce :: (Solving es, Eff.Writer Solver.Constraint :> es)  => AST.AST Elaborated NT.Type -> AST.AST Elaborated NT.Type -> Eff es (AST.AST Elaborated NT.Type)
reduce cons arg = case (node cons, node arg) of
    (T.Applied cons' arg', t) -> do
        reduced <- reduce cons' arg'
        reduce reduced arg
    (T.Polymorphic poly, t) -> do
        ev <- Shared.fresh E
        out <- Shared.fresh K

        let k = K.Arrow (AST.annotation arg) (AST.Raw $ K.Var out)
        let eq = Solver.Equality ev (Solver.K $ AST.extract cons) (Solver.K k)
        return $ AST.Annotated (instantiate poly t) (AST.Raw $ K.Var out)

    -- | TODO
    (T.Closure params tyExpr scope, t) -> _f







