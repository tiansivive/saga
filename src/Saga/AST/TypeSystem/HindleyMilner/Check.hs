module Saga.AST.TypeSystem.HindleyMilner.Check where
import           Control.Monad.Except                          (Except,
                                                                MonadError (throwError))
import           Control.Monad.Reader                          (ReaderT)
import           Control.Monad.RWS                             (RWST)
import qualified Data.Map                                      as Map
import           Saga.AST.TypeSystem.HindleyMilner.Constraints (HasKind (kind),
                                                                ProtocolEnv,
                                                                Subst,
                                                                isSubtype,
                                                                nullSubst,
                                                                occursCheck)

import           Saga.AST.TypeSystem.HindleyMilner.Environment (Infer, TypeEnv)
import           Saga.AST.TypeSystem.HindleyMilner.Inference   (infer)
import           Saga.AST.TypeSystem.HindleyMilner.Types       (BuiltInType (..),
                                                                Expr, Term (..),
                                                                Type (..),
                                                                Tyvar)





type Check = ReaderT ProtocolEnv (Except TypeCheckError)

data TypeCheckError
    = TypeMismatch Type Type
    | KindMismatch Tyvar Type
    | InfiniteType Tyvar Type

check :: Expr -> Type -> Infer Bool
check expr ty = do
    ty' <- infer expr
    return $ ty `matches` ty'



matches :: Type -> Type -> Bool
matches ty ty' = False


-- unify :: Type -> Type -> Check Subst
-- -- unify t1 t2 | trace ("Unifying:\n\t" ++ show t1 ++ "\n\t" ++ show t2) False = undefined
-- unify TUnit TUnit = return nullSubst

-- unify (TVar a) t  = bind a t
-- unify t (TVar a)  = bind a t

-- unify (TData a) (TData b) | a == b = return nullSubst

-- unify (TPrimitive a) (TPrimitive b) | a == b = return nullSubst
-- unify (TLiteral a) (TLiteral b) | a == b = return nullSubst

-- unify sub@(TRecord as) parent@(TRecord bs) = sub `isSubtype` parent
-- unify (TTuple as) (TTuple bs) = do
--   ss <- zipWithM unify as bs
--   return $ foldl compose nullSubst ss

-- unify (il `TArrow` ol) (ir `TArrow` or) = do
--   sub <- unify il ir
--   s <- apply sub ol `unify` apply sub or
--   return $ s `compose` sub


-- unify t t' = throwError $ TypeMismatch t t'

-- bind :: Tyvar -> Type -> Check Subst
-- --bind a t | trace ("Binding: " ++ show a ++ " to " ++ show t) False = undefined
-- bind a t
--   | t == TVar a = return nullSubst
--   | occursCheck a t = throwError $ InfiniteType a t
--   | kind a /= kind t = throwError $ KindMismatch a t
--   | TLiteral l <- t = return . Map.singleton a $
--       case l of
--         LInt _    -> TPrimitive TInt
--         LString _ -> TPrimitive TString
--         LBool _   -> TPrimitive TBool
--   | otherwise = return $ Map.singleton a t
