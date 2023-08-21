module Saga.AST.TypeSystem.HindleyMilner.Check where
import           Control.Monad.Except                          (Except,
                                                                MonadError (throwError),
                                                                runExcept)
import           Control.Monad.Reader                          (ReaderT (runReaderT))
import           Control.Monad.RWS                             (RWST)
import qualified Data.Map                                      as Map
import           Saga.AST.TypeSystem.HindleyMilner.Constraints (HasKind (kind),
                                                                ProtocolEnv,
                                                                Solve, Subst,
                                                                Substitutable (apply),
                                                                isSubtype,
                                                                nullSubst,
                                                                occursCheck,
                                                                runSolve, unify)

import           Control.Monad.Trans.RWS                       (evalRWST)
import           Data.Bifunctor                                (first)
import           Debug.Trace                                   (trace, traceM)
import           Saga.AST.TypeSystem.HindleyMilner.Environment (IConstraint,
                                                                Infer,
                                                                InferenceEnv,
                                                                InferenceError,
                                                                Scheme,
                                                                builtInProtocols,
                                                                empty,
                                                                initState)
import           Saga.AST.TypeSystem.HindleyMilner.Inference   (closeOver,
                                                                infer)
import           Saga.AST.TypeSystem.HindleyMilner.Types       (BuiltInType (..),
                                                                Expr, Term (..),
                                                                Type (..),
                                                                Tyvar)





type Check = ReaderT ProtocolEnv (Except InferenceError)

data TypeCheckError
    = TypeMismatch Type Type
    | KindMismatch Tyvar Type
    | InfiniteType Tyvar Type



run :: Expr -> Type-> Either String (Bool, [IConstraint])
run expr ty = show `first` runExcept (evalRWST (check expr ty) empty initState)


check :: Expr -> Type -> Infer Bool
check expr ty = do
    inferred <- infer expr
    inferred `matches` ty
    -- let solve = runExcept $ runReaderT ( ty `unify` ty') builtInProtocols
    -- return solve

-- | TODO: Need to have different unification rules here between type literals and primitives
-- | In addition, need to incorporate a mechanism for TClosure evaluation.
-- | Possibly replace all TIdentifiers, which refer to the params, in the TClosure body expression with TVars
matches :: Type -> Type -> Infer Bool
matches ty ty' | trace ("\nMatching:\n\tInferred: " ++ show ty ++ "\n\tSpecified: " ++ show ty') False = undefined
matches ty ty' = do
    sub <- ty `unify` ty'

    if Map.null sub then
        return True
    else do
        traceM $ "Substitution:\n\t" ++ show sub
        traceM $ "Subbed inferred:  " ++ show (apply sub ty)
        traceM $ "Subbed specified: " ++ show (apply sub ty')
        -- return $
        return True


