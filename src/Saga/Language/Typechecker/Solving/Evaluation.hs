{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}
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
import           Saga.Language.Typechecker.Errors                  (Exception (NotYetImplemented, Unexpected),
                                                                    SagaError (NoMatchingPattern),
                                                                    crash)
import qualified Saga.Language.Typechecker.Solving.Constraints     as Solver
import           Saga.Language.Typechecker.Solving.Monad           (Levels,
                                                                    Solving,
                                                                    Status (..))
import qualified Saga.Language.Typechecker.Solving.Shared          as Shared
import           Saga.Language.Typechecker.Solving.Shared          (Tag (..))
import           Saga.Utils.Operators                              ((|>), (||>))


import           Control.Applicative                               (Alternative ((<|>)))
import           Control.Monad                                     (foldM)
import           Data.Functor                                      ((<&>))
import           Data.List                                         (find)
import qualified Data.Map                                          as Map
import qualified Effectful.Error.Static                            as Eff
import           Saga.Language.Syntax.Polymorphism                 (Polymorphic (..))
import qualified Saga.Language.Syntax.Reduced.Types                as RD
import           Saga.Language.Typechecker.Elaboration.Types.Types




-- pattern TypeComputation body params arg = T.Applied (node -> (T.Computed params body)) arg


solve :: forall es. Solving es => Solver.Constraint -> Eff es (Status, Solver.Constraint)
solve c@(Solver.Evaluate result ty) = case ty of

    T.Applied (node -> T.Polymorphic poly) (node -> arg) -> do
        let t' = instantiate poly arg
        return (Solved, Solver.Evaluate result t')

    T.Applied (node -> T.Var tvar) _ -> return (Deferred, c)

    T.Match (node -> (T.Var _)) _ -> return (Deferred, c)
    T.Match subject cases -> matched >>= \case
        Nothing -> Eff.throwError $ NoMatchingPattern (node subject) (node <$> cases)
        Just (node -> ty') -> return (Solved, Solver.Evaluate result ty')


        where

            matched = cases ||> foldM (\ty ast -> fmap (ty <|>) (match' ast)) Nothing
            match' = node |> match (node subject)

            match (T.Singleton lit) (T.Case (node -> (T.PatLit lit')) body) | lit == lit'       = return $ Just body
            match (T.Data name)     (T.Case (node -> (T.Id name')) body)    | name == name'     = return $ Just body

            match t (T.Case (node -> T.Wildcard) body) = return $ Just body

            match t pat = crash . NotYetImplemented $ "Pattern matching: \nType: " <> show t <> "\nPattern: " <> show pat

    _ -> return (Solved, c)


solve c = crash $ Unexpected c "for Evaluate constraint"




simplify ::Solving es => Solver.Constraint -> Eff es Solver.Constraint
simplify c@(Solver.Evaluate result ty) = case ty of
    T.Applied {} -> return c
    T.Match {} -> return c

    _ -> do
        ev <- Shared.fresh E
        return (Solver.Equality ev (Solver.Ty $ node result) (Solver.Ty ty))







