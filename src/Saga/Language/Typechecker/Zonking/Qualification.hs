{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE LambdaCase #-}
module Saga.Language.Typechecker.Zonking.Qualification where

import qualified Saga.Language.Typechecker.Solving.Constraints as Solver



import           Data.Functor                                  ((<&>))
import qualified Data.Map                                      as Map
import           Data.Maybe                                    (mapMaybe)
import qualified Data.Set                                      as Set
import           Effectful                                     (Eff, (:>))
import qualified Effectful.Error.Static                        as Eff
import           Saga.Language.Typechecker.Errors              (Exception (NotYetImplemented),
                                                                SagaError (..),
                                                                crash)

import qualified Effectful                                     as Eff
import qualified Effectful.State.Static.Local                  as Eff



import           Saga.Language.Typechecker.Substitution        (Substitutable (..))

import qualified Saga.Language.Syntax.AST                      as NT (NodeType (..))
import           Saga.Language.Syntax.AST                      (AST,
                                                                Phase (Elaborated))
import qualified Saga.Language.Syntax.Elaborated.AST           as AST
import qualified Saga.Language.Syntax.Elaborated.Types         as T
import qualified Saga.Language.Syntax.Elaborated.Values        as AST
import           Saga.Utils.Operators                          ((||>))

import           Saga.Language.Syntax.Polymorphism             (Given (..),
                                                                Polymorphic (..),
                                                                Qualified (..))
import           Saga.Language.Typechecker.Traversals
import           Saga.Language.Typechecker.Zonking.Monad       (Zonking)



qualify :: Zonking es => AST Elaborated NT.Expression -> [Solver.Constraint] -> Eff es T.Type
qualify expr residuals = case expr of
    AST.Annotated expr ann -> qualify' ann

    e                      -> Eff.throwError $ UntypedInferredExpr e
    where

        qualify' (AST.Annotated ty k) = do
            --poly@(Forall tvars (bs :| cs :=> t)) <- Eff.evalState @Int 0 $ generalize ty
            return (T.Polymorphic $ Forall (Set.toList $ ftv ty) (T.Qualified $ Map.empty :| constraints :=> ty))

        constraints = residuals ||> mapMaybe (\case
                Solver.Empty -> Nothing
                Solver.Implementation ev ty pid -> Just $ ty `T.Implements` pid
                Solver.Refinement scope ty liquid -> Just $ T.Refinement scope liquid ty
                constraint -> crash . NotYetImplemented $ "Constraint qualification for: " ++ show constraint
                )
