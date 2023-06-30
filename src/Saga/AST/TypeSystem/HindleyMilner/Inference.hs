

{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}

{-# LANGUAGE StandaloneDeriving #-}


module Saga.AST.TypeSystem.HindleyMilner.Inference where

import           Control.Monad.Except
import           Control.Monad.State.Lazy                      (MonadState (get, put),
                                                                State,
                                                                evalState,
                                                                replicateM)
import           Control.Monad.Trans.Except                    (ExceptT,
                                                                runExceptT)
import           Data.List                                     (nub)
import qualified Data.Map                                      as Map
import qualified Data.Set                                      as Set
import           Saga.AST.Syntax                               (Name (..))
import           Saga.AST.TypeSystem.HindleyMilner.Environment
import           Saga.AST.TypeSystem.HindleyMilner.Types
import           Saga.Parser.ParsingInfo

-- import           Control.Monad.State.Lazy
-- import           Control.Monad.Trans.Except    (ExceptT)
-- import qualified Data.Map                      as Map
-- import qualified Data.Set                      as Set
-- import           Saga.AST.Syntax               (Expr, Name (..))
-- import           Saga.AST.TypeSystem.Inference hiding (Infer, TypeError, fresh,
--                                                 letters)
-- import           Saga.AST.TypeSystem.Types






-- runInfer :: Infer (Subst, Type) -> Either TypeError Scheme
-- runInfer m = case evalState (runExceptT m) initUnique of
--   Left err  -> Left err
--   Right res -> Right $ closeOver res

-- closeOver :: (Map.Map TVar Type, Type) -> Scheme
-- closeOver (sub, ty) = normalize sc
--   where sc = generalize emptyTyEnv (apply sub ty)

-- initUnique :: Unique
-- initUnique = Unique { count = 0 }

lookupVar :: TVar -> Infer (Subst, Type)
lookupVar v = do
    (Env vars _ _) <- get
    case Map.lookup v vars of
        Nothing -> throwError $ UnboundVariable v
        Just s  -> do
            t <- instantiate s
            return (nullSubst, t)



instantiate :: Scheme -> Infer Type
instantiate (Scheme as t) = do
  as' <- mapM (const fresh) as
  let s = Map.fromList $ zip as as'
  return $ apply s t

-- extend :: TypeEnv -> (TVar, Scheme) -> TypeEnv
-- extend e@(Env vars aliases) (var, scheme) = e{ typeVars = Map.insert var scheme vars }

-- emptyTyEnv :: TypeEnv
-- emptyTyEnv = Env { typeVars = Map.empty, typeAliases = Map.empty }

-- typeof :: TypeEnv -> TVar -> Maybe Scheme
-- typeof e@(Env vars aliases) var = Map.lookup var vars


class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> Set.Set TVar

instance Substitutable a => Substitutable [a] where
  apply  = fmap . apply
  ftv   = foldr (Set.union . ftv) Set.empty


instance Substitutable Type where
--   apply _ (TCon a)       = TCon a
  apply s t@(TVar id)     = Map.findWithDefault t id s
  apply s (t `TArrow` t') = apply s t `TArrow` apply s t'

--   ftv TCon{}         = Set.empty
  ftv (TVar id)       = Set.singleton id
  ftv (t `TArrow` t') = ftv t `Set.union` ftv t'

instance Substitutable Scheme where
  apply s (Scheme as t)   = Scheme as $ apply s' t
                            where s' = foldr Map.delete s as
  ftv (Scheme as t) = ftv t `Set.difference` Set.fromList as



-- instance Substitutable TypeEnv where
--   apply s e@(Env vars aliases) =  e{ typeVars = Map.map (apply s) vars }
--   ftv (Env vars aliases) = ftv $ Map.elems vars




-- compose :: Subst a -> Subst a -> Subst a
-- s `compose` s' = Map.map (apply s) s' `Map.union` s

-- -- unify ::  Type -> Type -> Infer Subst
-- -- unify (l `TArr` r) (l' `TArr` r')  = do
-- --   s <- unify l l'
-- --   s' <- unify (apply s r) (apply s r')
-- --   return (s' `compose` s)

-- -- unify (TVar a) t = bind a t
-- -- unify t (TVar a) = bind a t
-- -- unify (TCon a) (TCon b) | a == b = return nullSubst
-- -- unify t t' = throwError $ UnificationFail t t'

-- -- bind ::  TVar -> Type -> Infer Subst
-- -- bind a t
-- --   | t == TVar a     = return nullSubst
-- --   | occursCheck a t = throwError $ InfiniteType t
-- --   | otherwise       = return $ Map.singleton a t

-- -- occursCheck ::  Substitutable a => TVar -> a -> Bool
-- -- occursCheck a t = a `Set.member` ftv t



generalize :: TypeEnv -> Type -> Scheme
generalize env t  = Scheme as t
  where as = Set.toList $ ftv t `Set.difference` ftv env

-- -- ops :: Binop -> Type
-- -- ops Add = typeInt `TArr` typeInt `TArr` typeInt
-- -- ops Mul = typeInt `TArr` typeInt `TArr` typeInt
-- -- ops Sub = typeInt `TArr` typeInt `TArr` typeInt
-- -- ops Eql = typeInt `TArr` typeInt `TArr` typeBool

-- lookupEnv :: TypeEnv -> TVar -> Infer a (Subst a, Type)
-- lookupEnv (Env env) x =
--   case Map.lookup x env of
--     Nothing -> throwError $ UnboundVariable (show x)
--     Just s  -> do t <- instantiate s
--                   return (nullSubst, t)

-- -- infer :: TypeEnv -> Expr -> Infer (Subst, Type)
-- -- infer env ex = case ex of

-- --   Var x -> lookupEnv env x

-- --   Lam x e -> do
-- --     tv <- fresh
-- --     let env' = env `extend` (x, Forall [] tv)
-- --     (s, t) <- infer env' e
-- --     return (s, apply s tv `TArr` t)

-- --   App e e' -> do
-- --     tv <- fresh
-- --     (s, t) <- infer env e
-- --     (s', t') <- infer (apply s env) e'
-- --     s3       <- unify (apply s' t) (TArr t' tv)
-- --     return (s3 `compose` s' `compose` s, apply s3 tv)

-- --   Let x e e' -> do
-- --     (s, t) <- infer env e
-- --     let env' = apply s env
-- --         t'   = generalize env' t
-- --     (s', t') <- infer (env' `extend` (x, t')) e'
-- --     return (s' `compose` s, t')

-- --   If cond tr fl -> do
-- --     tv <- fresh
-- --     inferPrim env [cond, tr, fl] (typeBool `TArr` tv `TArr` tv `TArr` tv)

-- --   Fix e -> do
-- --     tv <- fresh
-- --     inferPrim env [e] ((tv `TArr` tv) `TArr` tv)

-- --   Op op e e' -> do
-- --     inferPrim env [e, e'] (ops op)

-- --   Lit (LInt _)  -> return (nullSubst, typeInt)
-- --   Lit (LBool _) -> return (nullSubst, typeBool)

-- -- inferPrim :: TypeEnv -> [Expr] -> Type -> Infer (Subst, Type)
-- -- inferPrim env l t = do
-- --   tv <- fresh
-- --   (s, tf) <- foldM inferStep (nullSubst, id) l
-- --   s' <- unify (apply s (tf tv)) t
-- --   return (s' `compose` s, apply s' tv)
-- --   where
-- --   inferStep (s, tf) exp = do
-- --     (s', t) <- infer (apply s env) exp
-- --     return (s' `compose` s, tf . (TArr t))

-- -- inferExpr :: TypeEnv -> Expr -> Either TypeError Scheme
-- -- inferExpr env = runInfer . infer env

-- -- inferTop :: TypeEnv -> [(String, Expr)] -> Either TypeError TypeEnv
-- -- inferTop env [] = Right env
-- -- inferTop env ((name, ex):xs) = case inferExpr env ex of
-- --   Left err -> Left err
-- --   Right ty -> inferTop (extend env (name, ty)) xs

normalize :: Scheme -> Scheme
normalize (Scheme ts body) = Scheme (fmap snd ord) (normtype body)
  where
    ord = zip (nub $ fv body) ( letters)

    fv (TVar a)   = [a]
    -- fv (TArr a b) = fv a ++ fv b
    -- fv (TCon _)   = []

    -- normtype (TArr a b) = TArr (normtype a) (normtype b)
    -- normtype (TCon a)   = TCon a
    normtype (TVar a)   =
      case lookup a ord of
        Just x  -> TVar x
        Nothing -> error "type variable not in signature"


