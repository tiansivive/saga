{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Saga.AST.TypeSystem.HindleyMilner.Inference where

import           Control.Monad.Except
import           Control.Monad.RWS                             (MonadReader (ask, local),
                                                                MonadWriter (tell),
                                                                RWST (runRWST),
                                                                evalRWST)
import           Control.Monad.State.Lazy                      (MonadState,
                                                                State,
                                                                evalState,
                                                                evalStateT,
                                                                replicateM)
import           Control.Monad.Trans.Except                    (ExceptT,
                                                                runExceptT)
import           Data.Bifunctor                                (Bifunctor (first))
import           Data.Functor                                  ((<&>))
import           Data.List                                     (nub, partition,
                                                                (\\))
import qualified Data.Map                                      as Map
import qualified Data.Set                                      as Set
import           Debug.Trace                                   (trace, traceM)
import           Saga.AST.TypeSystem.HindleyMilner.Constraints
import           Saga.AST.TypeSystem.HindleyMilner.Environment
import           Saga.AST.TypeSystem.HindleyMilner.Types
import           Saga.Parser.ParserHM                          (runSagaExpr)
import           Saga.Parser.ParsingInfo





run :: String -> Either String Scheme
run input = do
  Parsed expr _ _ <- runSagaExpr input
  show `first` runInfer (infer expr)

runInfer :: Infer Type -> Either InferenceError Scheme
runInfer m = do
  (ty, constraints) <- runExcept $ evalRWST m empty initState
  subst <- runSolve constraints
  return $ closeOver $ apply subst ty



closeOver :: Type -> Scheme
closeOver =  normalize . generalize empty

lookupVar :: UnificationVar -> Infer (Subst, Type)
lookupVar v = do
  (Env vars _) <- ask
  case Map.lookup v vars of
    Nothing -> throwError $ UnboundVariable v
    Just s -> do
      t <- instantiate s
      return (nullSubst, t)

instantiate :: Scheme -> Infer Type
instantiate sc | trace ("Instantiating: " ++ show sc) False = undefined
instantiate (Scheme as ( _ :=> t)) = do
  as' <- mapM (const fresh) as
  let s = Map.fromList $ zip as as'
  traceM $ "Zipped: " ++ show s
  return $ apply s t


generalize :: TypeEnv -> Type -> Scheme
generalize env t
  | trace ("Generalizing: " ++ show t
  ++ "\n\tEnv: " ++ show env
  ++ "\n\n\tFTV ty: " ++ show (ftv t)
  ++ "\n\tFTV env: " ++ show (ftv env)) False = undefined
generalize env t = Scheme as ( [] :=> t)
  where as = Set.toList $ ftv t `Set.difference` ftv env



lookupEnv :: UnificationVar -> Infer Type
lookupEnv x = do
  (Env vars aliases) <- ask
  case Map.lookup x aliases of
    Just ty -> return ty
    Nothing -> case Map.lookup x vars of
      Nothing -> throwError $ UnboundVariable (show x)
      Just s  -> instantiate s


infer :: Expr -> Infer Type
infer ex | trace ("Inferring: " ++ show ex) False = undefined
infer ex = case ex of
  Identifier x -> lookupEnv x
  Lambda (param : rest) body -> do
    tVar <- fresh
    out' <- infer out `scoped` (param, Scheme [] ( [] :=> tVar))
    return $ tVar `TArrow` out'
    where
      out = case rest of
        [] -> body
        _  -> Lambda rest body

  FnApp fn [arg] -> do
    out <- fresh
    fnTy <- infer fn
    argTy <- infer arg
    let inferred = argTy `TArrow` out
    emit $ fnTy `Equals` inferred
    return out

  FnApp fn (a : as) -> infer curried
    where
      partial = FnApp fn [a]
      curried = foldl (\f a -> FnApp f [a]) partial as

  -- Assign x e -> infer $ Lambda [x] e
    -- t <- infer e
    -- t' <- generalize t
    -- let scope
    -- modify $ \env -> env `extend` (x, t')
    -- return (s, t)

  IfElse cond yes no -> do
    cond' <- infer cond
    yes' <- infer yes
    no' <- infer no
    emit $ cond' `Equals` TPrimitive TBool
    -- | TODO: this should change to a union type when those get implemented
    emit $ yes' `Equals` no'
    return yes'

  Tuple elems -> do
    tElems <- mapM infer elems
    return $ TTuple tElems

  Record pairs -> do
    tPairs <- mapM infer' pairs
    return $ TRecord tPairs
    where
      infer' = mapM infer

  Term (LInt i) -> return $ TLiteral (LInt i)
  Term (LBool b) -> return $ TLiteral (LBool b)
  Term (LString s) -> return $ TLiteral (LString s)



normalize :: Scheme ->  Scheme
normalize sc | trace ("Normalizing: " ++ show sc) False = undefined
normalize (Scheme _ ( _ :=> body)) = Scheme (fmap snd ord) ([] :=> ty)
  where
    ty = normtype body
    ord = zip (nub $ fv body) letters

    fv (TVar a)       = [a]
    fv (a `TArrow` b) = fv a ++ fv b
    fv _              = []

    normtype (a `TArrow` b) = normtype a `TArrow` normtype b
    normtype (TVar a) = case lookup a ord of
        Just x  -> TVar x
        Nothing -> error "type variable not in signature"
    normtype ty = ty




