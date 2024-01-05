{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE LambdaCase #-}
module Saga.Language.Typechecker.Zonking.Qualification where
import qualified Saga.Language.Core.Expr                                 as AST
import qualified Saga.Language.Typechecker.Solver.Constraints            as Solver
import           Saga.Language.Typechecker.Type                          (Polymorphic,
                                                                          Scheme (..),
                                                                          Type)


import           Data.Functor                                            ((<&>))
import qualified Data.Map                                                as Map
import           Data.Maybe                                              (mapMaybe)
import qualified Data.Set                                                as Set
import           Effectful                                               (Eff,
                                                                          (:>))
import qualified Effectful.Error.Static                                  as Eff
import           Saga.Language.Typechecker.Errors                        (Exception (NotYetImplemented),
                                                                          SagaError (..),
                                                                          crash)

import qualified Effectful                                               as Eff
import qualified Effectful.State.Static.Local                            as Eff
import           Saga.Language.Typechecker.Inference.Inference           (Generalize (generalize))
import           Saga.Language.Typechecker.Inference.Type.Generalization
import qualified Saga.Language.Typechecker.Inference.Type.Shared         as Shared
import           Saga.Language.Typechecker.Monad                         (TypeCheck)
import qualified Saga.Language.Typechecker.Qualification                 as Q
import           Saga.Language.Typechecker.Qualification                 (Given (..),
                                                                          Qualified (..))
import           Saga.Language.Typechecker.Solver.Substitution           (Substitutable (..))
import qualified Saga.Language.Typechecker.Type                          as T
import           Saga.Utils.Operators                                    ((||>))


type QualifierEff es = TypeCheck es

qualify :: QualifierEff es => AST.Expr -> [Solver.Constraint] -> Eff es (Polymorphic Type)
qualify expr residuals = case expr of
    AST.Lambda evidence (AST.Typed e ty)    -> qualify' ty
    AST.Typed e ty                          -> qualify' ty
    e                                       -> Eff.throwError $ UntypedInferredExpr e
    where

        qualify' ty = do
            --poly@(Forall tvars (bs :| cs :=> t)) <- Eff.evalState @Int 0 $ generalize ty
            return ( Forall (Set.toList $ ftv ty) (Map.empty :| constraints :=> ty))

        constraints = residuals ||> mapMaybe (\case
                Solver.Empty -> Nothing
                Solver.Impl ev it pid -> Just $ fromItem it `Q.Implements` pid
                Solver.Refined scope it liquid -> Just $ Q.Refinement (fmap fromItem scope) liquid (fromItem it)
                constraint -> crash . NotYetImplemented $ "Constraint qualification for: " ++ show constraint
                )

        fromItem (Solver.Mono ty) = ty
        fromItem (Solver.Var tvar) = T.Var tvar

        fromItem it =  crash . NotYetImplemented $ "Constraint qualification: fromItem " ++ show it


