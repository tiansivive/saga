{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE LambdaCase #-}
module Saga.Language.Typechecker.Zonking.Qualification where
import qualified Saga.Language.Core.Expr                       as AST
import qualified Saga.Language.Typechecker.Solver.Constraints  as Solver
import           Saga.Language.Typechecker.Type                (Polymorphic,
                                                                Scheme (..),
                                                                Type)


import           Data.Functor                                  ((<&>))
import qualified Data.Map                                      as Map
import           Data.Maybe                                    (mapMaybe)
import qualified Data.Set                                      as Set
import qualified Effectful.Error.Static                        as Eff
import           Saga.Language.Typechecker.Errors              (Exception (NotYetImplemented),
                                                                SagaError (..),
                                                                crash)
import           Saga.Language.Typechecker.Monad               (TypeCheck)
import qualified Saga.Language.Typechecker.Qualification       as Q
import           Saga.Language.Typechecker.Qualification       (Given (..),
                                                                Qualified (..))
import           Saga.Language.Typechecker.Solver.Substitution (Substitutable (..))
import           Saga.Utils.Operators                          ((||>))


type Qualifier = TypeCheck '[Eff.Error SagaError]

qualify :: AST.Expr -> [Solver.Constraint] -> Qualifier (Polymorphic Type)
qualify expr residuals = case expr of

    AST.Lambda evidence (AST.Typed e ty)    -> return $ Forall (Set.toList $ ftv ty) (Map.empty :| constraints :=> ty)
    AST.Typed e ty                          -> return $ Forall (Set.toList $ ftv ty) (Map.empty :| constraints :=> ty)
    e                                       -> Eff.throwError $ UntypedInferredExpr e
    where
        constraints = residuals ||> mapMaybe (\case
                Solver.Empty -> Nothing
                Solver.Impl ev (Solver.Mono ty) pid -> Just $ ty `Q.Implements` pid
                constraint -> crash . NotYetImplemented $ "Constraint qualification for: " ++ show constraint
                )



