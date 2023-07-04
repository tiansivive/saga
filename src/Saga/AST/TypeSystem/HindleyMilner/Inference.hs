

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}




module Saga.AST.TypeSystem.HindleyMilner.Inference where

import           Control.Monad.Except
import           Control.Monad.State.Lazy                      (MonadState,
                                                                State,
                                                                evalState,
                                                                evalStateT,
                                                                replicateM)
import           Control.Monad.Trans.Except                    (ExceptT,
                                                                runExceptT)
import           Data.Bifunctor                                (Bifunctor (first))
import           Data.List                                     (nub)
import qualified Data.Map                                      as Map
import qualified Data.Set                                      as Set
import           Saga.AST.Syntax                               (Name (..))
import           Saga.AST.TypeSystem.HindleyMilner.Environment
import           Saga.AST.TypeSystem.HindleyMilner.Refinement  (refine)
import           Saga.AST.TypeSystem.HindleyMilner.Types
import           Saga.Parser.Parser                            (runSagaExpr)
import           Saga.Parser.ParsingInfo

-- import           Control.Monad.State.Lazy
-- import           Control.Monad.Trans.Except    (ExceptT)
-- import qualified Data.Map                      as Map
-- import qualified Data.Set                      as Set
-- import           Saga.AST.Syntax               (Expr, Name (..))
-- import           Saga.AST.TypeSystem.Inference hiding (Infer, TypeError, fresh,
--                                                 letters)
-- import           Saga.AST.TypeSystem.Types



-- run :: String -> Either String Scheme
-- run input = do
--     parsed <- runSagaExpr input
--     show `first` runInfer (infer empty parsed)







runInfer :: Infer (Subst, Type) -> Either TypeError Scheme
runInfer m = runExcept $ evalStateT inference [empty]
  where
    inference = do
      res <- m
      scheme <- closeOver res
      return $ normalize scheme


closeOver :: (Map.Map TVar Type, Type) -> Infer Scheme
closeOver (sub, ty) = do
  sub' <- apply sub ty
  scheme <- generalize empty sub'
  return $ normalize scheme



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
  apply s t

extend :: TypeEnv -> (TVar, Scheme) -> TypeEnv
extend e@(Env vars aliases count) (var, scheme) = e{ typeVars = Map.insert var scheme vars }

-- emptyTyEnv :: TypeEnv
-- emptyTyEnv = Env { typeVars = Map.empty, typeAliases = Map.empty }

-- typeof :: TypeEnv -> TVar -> Maybe Scheme
-- typeof e@(Env vars aliases) var = Map.lookup var vars


class  Substitutable f a where
  apply :: Subst -> a -> f a
  ftv   :: a -> f (Set.Set TVar)

instance (Monad f, Substitutable f a) => Substitutable f [a] where
  apply  = mapM . apply
  ftv = foldM union Set.empty
    where
      union set x = do
        set' <- ftv x
        return $ Set.union set' set



instance Substitutable Infer Type where
--   apply _ (TCon a)       = TCon a
  apply s t@(TVar id)     = return $ Map.findWithDefault t id s
  apply s (inExpr `TArrow` outExpr) = do
    inTy <- refine inExpr
    outTy <- refine outExpr
    in' <- apply s inTy
    out' <- apply s outTy

    return $ Type in' `TArrow` Type out'

--   ftv TCon{}         = Set.empty
  ftv (TVar id)       = return $ Set.singleton id
  ftv (t `TArrow` t') = do
    set <- refine t >>= ftv
    set' <- refine t' >>= ftv
    return $ set `Set.union` set'

instance Substitutable Infer Scheme where
  apply s (Scheme as t) =
    let
      s' = foldr Map.delete s as
    in do
      ty <- apply s' t
      return $ Scheme as ty


  ftv (Scheme as t) = do
    set <- ftv t
    return $ set `Set.difference` Set.fromList as



instance Substitutable Infer TypeEnv where
  apply s e@(Env vars aliases count) = do
    typeVars' <- sequence $ Map.map (apply s) vars
    return $ e{ typeVars = typeVars' }
  ftv (Env vars aliases count) = ftv $ Map.elems vars




compose :: Subst -> Subst  -> Infer Subst
s `compose` s' = do
  s'' <- sequence $ Map.map (apply s) s'
  return $ s'' `Map.union` s

unify ::  Type -> Type -> Infer Subst
unify (lExpr `TArrow` rExpr) (lExpr' `TArrow` rExpr')  = do
  l <- refine lExpr
  l' <- refine lExpr'
  r <- refine rExpr
  r' <- refine rExpr'

  s <- unify l l'
  t  <- apply s r
  t'  <- apply s r'
  s' <- unify t t'

  s' `compose` s

unify (TVar a) t = bind a t
unify t (TVar a) = bind a t
-- unify (TCon a) (TCon b) | a == b = return nullSubst
unify t t' = throwError $ UnificationFail t t'

bind ::  TVar -> Type -> Infer Subst
bind a t
  | t == TVar a     = return nullSubst
  | otherwise       = do
    s <- occursCheck a t
    if s
      then throwError $ InfiniteType a t
      else return $ Map.singleton a t

occursCheck :: Substitutable Infer a => TVar -> a -> Infer Bool
occursCheck a t = do
  set <- ftv t
  return $ a `Set.member` set


generalize :: TypeEnv -> Type -> Infer Scheme
generalize env t = do
    t' <- ftv t
    env' <- ftv env
    let as = Set.toList $ t' `Set.difference` env'
    return $ Scheme as t



-- -- ops :: Binop -> Type
-- -- ops Add = typeInt `TArr` typeInt `TArr` typeInt
-- -- ops Mul = typeInt `TArr` typeInt `TArr` typeInt
-- -- ops Sub = typeInt `TArr` typeInt `TArr` typeInt
-- -- ops Eql = typeInt `TArr` typeInt `TArr` typeBool

lookupEnv :: TypeEnv -> TVar -> Infer (Subst, Type)
lookupEnv (Env vars aliases count) x =
  case Map.lookup x vars of
    Nothing -> throwError $ UnboundVariable (show x)
    Just s  -> do t <- instantiate s
                  return (nullSubst, t)

infer :: TypeEnv -> Expr -> Infer (Subst, Type)
infer env ex = case ex of

  Identifier x -> lookupEnv env x

  -- Lambda _ x e -> do
  --   tv <- fresh
  --   let env' = env `extend` (x, Scheme [] tv)
  --   (s, t) <- infer env' e
  --   return (s, apply s tv `TArrow` t)
  -- Lambda info [arg] body -> do
  --   tvar <- fresh
  --   let env' = env `extend` (arg, Scheme [] tvar)
  --   (s, t) <- infer env' body
  --   t' <- apply s tvar
  --   return (s, Type info t' `TArrow` Type info t) -- TODO: do this recursively

  Lambda (param:rest) body -> do
    tvar <- fresh
    let env' = env `extend` (param, Scheme [] tvar)
    (s, t) <- infer env' $ case rest of
      [] -> body
      _  -> Lambda rest body
    t' <- apply s tvar
    return (s, Type t' `TArrow` Type t) -- TO


  FnApp fn (arg:rest) -> do
    tv <- fresh
    (s, t) <- infer env fn
    env' <- apply s env
    (s', t') <- infer env' $ case rest of
      [body] -> body
      _      -> FnApp arg rest
    -- (s', t') <- infer env' e'
    ty <- apply s' t
    s3       <- unify ty (Type t' `TArrow` Type tv)
    sub <- s3 `compose` s'
    sub' <- sub `compose` s
    ty' <- apply s3 tv
    return (sub, ty')
-- Let x e1 e2 -> do
--     (s1, t1) <- infer env e1
--     let env' = apply s1 env
--         t'   = generalize env' t1
--     (s2, t2) <- infer (env' `extend` (x, t')) e2
--     return (s2 `compose` s1, t2)
  Assign x e -> do
    (s, t) <- infer env e
    env' <- apply s env
    t'   <- generalize env' t
    let env'' = env' `extend` (x, t')
    -- (s', t') <- infer (env' `extend` (x, t')) e'

    return (s, t)

  IfElse cond tr fl -> do
    tv <- fresh
    let tArrow t t' = Type t `TArrow` Type t'
    inferPrim env [cond, tr, fl] $ TPrimitive TBool `tArrow` tv `tArrow` tv `tArrow` tv
  -- Fix e -> do
  --   tv <- fresh
  --   inferPrim env [e] ((tv `TArr` tv) `TArr` tv)

  -- Op op e e' -> do
  --   inferPrim env [e, e'] (ops op)

  Term (LInt i)  -> return (nullSubst, TLiteral (LInt i))
  Term (LBool b) -> return (nullSubst, TLiteral (LBool b))
  Term (LString s) -> return (nullSubst, TLiteral (LString s))


inferPrim :: TypeEnv -> [Expr] -> Type -> Infer (Subst, Type)
inferPrim env l t = do
  tv <- fresh
  (s, tf) <- foldM inferStep (nullSubst, id) l
  t' <- apply s (tf tv)
  s' <- unify t' t
  sub <- s' `compose` s
  t'' <-  apply s' tv
  return (sub, t'')
    where
      inferStep (s, tf) exp = do
        env' <- apply s env
        (s', t) <- infer env' exp
        sub <- s' `compose` s
        return (sub, tf . TArrow (Type t) . Type)

inferExpr :: TypeEnv -> Expr -> Either TypeError Scheme
inferExpr env = runInfer . infer env

inferTop :: TypeEnv -> [(String, Expr)] -> Either TypeError TypeEnv
inferTop env [] = Right env
inferTop env ((name, ex):xs) = case inferExpr env ex of
  Left err -> Left err
  Right ty -> inferTop (extend env (name, ty)) xs

normalize :: Scheme -> Scheme
normalize (Scheme ts body) = Scheme (fmap snd ord) (normtype body)
  where
    ord = zip (nub $ fv body) letters

    fv (TVar a)   = [a]
    -- fv (TArr a b) = fv a ++ fv b
    -- fv (TCon _)   = []

    -- normtype (TArr a b) = TArr (normtype a) (normtype b)
    -- normtype (TCon a)   = TCon a
    normtype (TVar a)   =
      case lookup a ord of
        Just x  -> TVar x
        Nothing -> error "type variable not in signature"


