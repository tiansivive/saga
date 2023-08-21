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


-- matches :: Type -> Type-> Check ()
-- matches ty ty' = do
--     (subst, implConstraints) <- runSolve []
--     return $ closeOver implConstraints $ apply subst ty



-- unify :: Type -> Type -> Check Subst
-- --unify t1 t2 | trace ("Unifying:\n\t" ++ show t1 ++ "\n\t" ++ show t2) False = undefined
-- unify (il `TArrow` ol) (ir `TArrow` or) = do
--   sub <- unify il ir
--   s <- apply sub ol `unify` apply sub or
--   return $ s `compose` sub
-- unify (TApplied f t) (TApplied f' t') = do
--   sub <- unify f f'
--   s <- apply sub t `unify` apply sub t'
--   return $ s `compose` sub

-- unify (TData lCons) (TData rCons) | lCons == rCons = return nullSubst
-- unify (TVar a) t = bind a t
-- unify t (TVar a) = bind a t
-- unify (TPrimitive a) (TPrimitive b) | a == b = return nullSubst
-- unify (TLiteral a) (TLiteral b) | a == b = return nullSubst
-- unify (TTuple as) (TTuple bs) = do
--   ss <- zipWithM unify as bs
--   return $ foldl compose nullSubst ss
-- unify sub@(TRecord as) parent@(TRecord bs) = sub `isSubtype` parent


-- unify t t' | kind t /= kind t' = throwError $ Fail "Kind mismatch"
-- unify t t' = throwError $ UnificationFail t t'

-- bind :: Tyvar -> Type -> Solve Subst
-- bind a t | trace ("Binding: " ++ show a ++ " to " ++ show t) False = undefined
-- bind a t
--   | t == TVar a = return nullSubst
--   | occursCheck a t = throwError $ InfiniteType a t
--   | kind a /= kind t = throwError $ Fail "kinds do not match"
--   | TLiteral l <- t = return . Map.singleton a $
--       case l of
--         LInt _    -> TPrimitive TInt
--         LString _ -> TPrimitive TString
--         LBool _   -> TPrimitive TBool
--   | otherwise = return $ Map.singleton a t
