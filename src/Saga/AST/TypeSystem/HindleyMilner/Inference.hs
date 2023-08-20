{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Saga.AST.TypeSystem.HindleyMilner.Inference where

import           Control.Applicative                           ((<|>))
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
import           Data.Maybe                                    (fromMaybe)
import qualified Data.Set                                      as Set
import           Debug.Trace                                   (trace, traceM)
import           Prelude                                       hiding (EQ)
import           Saga.AST.TypeSystem.HindleyMilner.Constraints
import           Saga.AST.TypeSystem.HindleyMilner.Environment
import qualified Saga.AST.TypeSystem.HindleyMilner.Types       as T
import           Saga.AST.TypeSystem.HindleyMilner.Types       hiding
                                                               (Implements)
import           Saga.Parser.ParserHM                          (runSagaExpr)
import           Saga.Parser.ParsingInfo

run :: String -> Either String Scheme
run input = do
  Parsed expr _ _ <- runSagaExpr input
  show `first` runInfer (infer expr)

runInfer :: Infer Type -> Either InferenceError Scheme
runInfer m = do
  traceM "\n\n"
  traceM "--------------------"
  traceM "RUNNING INFERENCE"
  traceM "\n\n"
  (ty, constraints) <- runExcept $ evalRWST m empty initState
  traceM "\n\n"
  traceM "--------------------"
  traceM "SOLVING CONSTRAINTS"
  traceM $ "Inferred Type: " ++ show ty
  traceM $ "Emitted Constraints" ++ show constraints
  traceM "\n\n"
  (subst, implConstraints) <- runSolve constraints
  traceM "\n\n"
  traceM "--------------------"
  traceM "CLOSING OVER"
  traceM $ "Subst: " ++ show subst
  traceM $ "Impl constraints" ++ show implConstraints
  traceM "\n\n"
  return $ closeOver implConstraints $ apply subst ty

closeOver :: [ImplConstraint] -> Type -> Scheme
closeOver cs = normalize . generalize empty cs

class Instantiate t where
  inst :: [Type] -> t -> t

instance Instantiate Type where
  -- inst ts (TAp l r) = TAp (inst ts l) (inst ts r)
  -- inst ts (TGen n)  = ts !! n
  inst ts t = t

instance Instantiate a => Instantiate [a] where
  inst ts = map (inst ts)

instance Instantiate t => Instantiate (Qualified t) where
  inst ts (cs :=> t) = inst ts cs :=> inst ts t

instance Instantiate Constraint where
  inst ts (t `T.Implements` p) = inst ts t `T.Implements` p


instantiate :: Scheme -> Infer Type
instantiate sc | trace ("Instantiating: " ++ show sc) False = undefined
instantiate (Scheme tvars qualified@(cs :=> t)) = do
  tVars <- mapM (fresh . getKind) vars
  let sub = Map.fromList $ zip vars tVars
  traceM $ "Zipped: " ++ show sub
  tell $ mkIConstraint <$> apply sub cs
  return $ apply sub t
    where
      getKind (Tyvar v k) = k
      vars = Set.toList $ ftv cs



generalize :: InferenceEnv -> [ImplConstraint] -> Type -> Scheme
generalize env impls t
  | trace
      ( "Generalizing: "
          ++ show t
          ++ "\n\tEnv: "
          ++ show env
          ++ "\n\tImplementation constraints: "
          ++ show impls
          ++ "\n\n\tFTV ty: "
          ++ show (ftv t)
          ++ "\n\tFTV env: "
          ++ show (ftv env)
      )
      False =
      undefined
generalize env impls t = Scheme [] (fmap mkConstraint impls :=> t)
  where

    mkConstraint (ty `IP` p) = ty `T.Implements` p


lookupEnv :: String -> Infer Type
lookupEnv x = do
  (Env vars aliases) <- ask
  case Map.lookup x aliases <|> Map.lookup (Tyvar x KType) vars of
    Just sc -> instantiate sc
    Nothing -> throwError $ UnboundVariable (show x)


infer :: Expr -> Infer Type
infer ex | trace ("Inferring: " ++ show ex) False = undefined
infer ex = case ex of
  Identifier x -> lookupEnv x

  Lambda (param : rest) body -> do
    tVar <- fresh KType
    out' <- infer out `scoped` (Tyvar param KType, Scheme (tvars tVar) ([] :=> tVar))
    return $ tVar `TArrow` out'
    where
      tvars (TVar v) = [v]
      out = case rest of
        [] -> body
        _  -> Lambda rest body
  FnApp fn [arg] -> do
    out <- fresh KType
    fnTy <- infer fn
    argTy <- infer arg
    genArg <- generalizeArg argTy

    -- foo <- argTy `unify`

    let inferred = genArg `TArrow` out
    emit $ EqCons $ fnTy `EQ` inferred
    --emit $ EqCons $ argTy `EQ` genArg
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
    emit $ EqCons $ cond' `EQ` TPrimitive TBool
    -- \| TODO: this should change to a union type when those get implemented
    return $ if yes' == no' then yes' else TUnion yes' no'

  Tuple elems -> do
    tElems <- mapM infer elems
    return $ TTuple tElems
  Record pairs -> do
    tPairs <- mapM infer' pairs
    return $ TRecord tPairs
    where
      infer' = mapM infer

  Term literal -> return $ TLiteral literal


generalizeArg :: Type -> Infer Type
generalizeArg (TLiteral lit) = generalize' $ case lit of
    LString _ -> ("IsString", TString)
    LBool _   -> ("IsBool", TBool)
    LInt _    -> ("Num", TInt)
    where
      generalize' (protocol, t) = do
        tVar <- fresh KType
        --emit $ EqCons $ tVar `EQ` TPrimitive t
        emit $ ImplCons $ tVar `IP` protocol
        return tVar

generalizeArg ty = return ty

normalize :: Scheme -> Scheme
normalize sc | trace ("Normalizing: " ++ show sc) False = undefined
normalize (Scheme k (cs :=> ty)) = Scheme k (cs' :=> ty')
  where
    ty' = normType ty
    cs' = normConstraint <$> cs
    ord = zip (nub $ fv ty) letters

    fv (TVar a)       = [a]
    fv (a `TArrow` b) = fv a ++ fv b
    fv _              = []

    normConstraint (t `T.Implements` p) = normType t `T.Implements` p

    normType (a `TArrow` b) = normType a `TArrow` normType b
    normType (TVar v) = case lookup v ord of
      Just x  -> TVar $ Tyvar x $ kind v
      Nothing -> error "type variable not in signature"
    normType t = t


