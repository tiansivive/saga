{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}


module Saga.Language.TypeSystem.HindleyMilner.Inference where

import           Control.Applicative                                ((<|>))
import           Control.Monad.Except
import           Control.Monad.RWS                                  (MonadReader (ask, local),
                                                                     MonadWriter (tell),
                                                                     RWST (runRWST),
                                                                     evalRWST,
                                                                     modify')
import           Control.Monad.State.Lazy                           (MonadState,
                                                                     State,
                                                                     evalState,
                                                                     evalStateT,
                                                                     replicateM)
import           Control.Monad.Trans.Except                         (ExceptT,
                                                                     runExceptT)
import           Data.Bifunctor                                     (Bifunctor (first))
import           Data.Functor                                       ((<&>))
import           Data.List                                          (nub,
                                                                     partition,
                                                                     (\\))
import qualified Data.Map                                           as Map
import           Data.Maybe                                         (fromMaybe)
import qualified Data.Set                                           as Set
import           Debug.Trace                                        (trace,
                                                                     traceM)
import           Prelude                                            hiding (EQ,
                                                                     log)
import           Saga.Language.Core.Literals                        (Literal (..))
import           Saga.Language.Core.Syntax
import           Saga.Language.TypeSystem.HindleyMilner.Constraints hiding
                                                                    (unification)
import           Saga.Language.TypeSystem.HindleyMilner.Environment
import qualified Saga.Language.TypeSystem.HindleyMilner.Types       as T
import           Saga.Language.TypeSystem.HindleyMilner.Types       hiding
                                                                    (Implements)

import           Saga.Language.TypeSystem.HindleyMilner.Shared
import           Saga.Parser.Desugar
import           Saga.Parser.Parser                                 (runSagaExpr)
import           Saga.Parser.Shared                                 hiding
                                                                    (Record,
                                                                     Term,
                                                                     Tuple,
                                                                     return)

import           Control.Monad.Trans.RWS                            (get,
                                                                     modify)
import           Saga.Language.TypeSystem.HindleyMilner.Errors      (SagaError (..))
import           Saga.Language.TypeSystem.HindleyMilner.Lib         (defaultEnv,
                                                                     listConstructor)
import qualified Saga.Language.TypeSystem.HindleyMilner.Refinement  as Refine


--type Infer a = Saga InferState (WriterT [IConstraint] (Except SagaError)) a
type Infer = RWST CompilerState [IConstraint] InferState (Except SagaError)
data InferState = IST { tvars :: Int, kvars:: Int, unification:: Map.Map String Tyvar  } deriving (Show)

run :: String -> Either String TypeExpr
run input = do
  Parsed expr _ _ <- runSagaExpr input
  show `first` runExcept (runInfer defaultEnv (infer $ desugarExpr expr))


runInfer :: CompilerState -> Infer Type -> Except SagaError TypeExpr
runInfer env m = do
  -- traceM "\n\n"
  -- traceM "--------------------"
  -- traceM "RUNNING INFERENCE"
  -- traceM "\n\n"
  (ty, constraints) <-  evalRWST m env initState
  -- traceM "\n\n"
  -- traceM "--------------------"
  -- traceM "SOLVING CONSTRAINTS"
  -- traceM $ "Inferred Type: " ++ show ty
  -- traceM $ "Emitted Constraints" ++ show constraints
  -- traceM "\n\n"
  (subst, implConstraints) <- runSolve env constraints
  -- traceM "\n\n"
  -- traceM "--------------------"
  -- traceM "CLOSING OVER"
  -- traceM $ "Subst: " ++ show subst
  -- traceM $ "Impl constraints" ++ show implConstraints
  -- traceM "\n\n"
  return $ closeOver implConstraints $ apply subst ty


-- inference :: Monad t => Expr -> Saga t (Maybe TypeExpr)
-- inference expr = case runInfer (infer expr) of
--   Left err -> do
--     log (Error $ show err)
--     return Nothing
--   Right te -> return $ Just te





closeOver :: [ImplConstraint] -> Type -> TypeExpr
closeOver cs = normalize . generalize empty cs

class Instantiate a where
  instantiate :: a -> Infer Type

-- instance (Instantiate a, Functor f) => Instantiate (f a) where
--   instantiate = mapM instantiate

instance Instantiate Type where

  instantiate (TClosure _ body _) = instantiate body
  instantiate (TApplied cons arg) = TApplied <$> instantiate cons <*> instantiate arg
  instantiate (TUnion tys) = TUnion <$> mapM instantiate tys
  instantiate (TTuple tys) = TTuple <$> mapM instantiate tys
  instantiate (TRecord pairs) = TRecord <$> mapM (mapM instantiate) pairs
  instantiate (TArrow inTy outTy) = TArrow <$> instantiate inTy <*> instantiate outTy
  instantiate ty = return ty

instance Instantiate TypeExpr where
  instantiate te | trace ("\n\n------------\nInstantiating: " ++ show te) False = undefined
  instantiate (TQualified (cs :=> te)) = do
    tVars <- mapM (fresh . getKind) vars
    let sub = Map.fromList $ zip vars tVars
    let te' = apply sub te

    -- traceM $ "\nZipped:\t" ++ show sub
    -- traceM $ "\nSubbed type expression:\t" ++ show te'
    tell $ mkIConstraint <$> apply sub cs

    instantiate te'
    where
      getKind (Tyvar v k) = k
      vars = Set.toList $ ftv cs


  instantiate tyExpr = do
    env <- ask
    case Refine.runIn env tyExpr of
      Right (TClosure _ body _ ) -> instantiate body
      Right ty                   -> return ty
      Left err                   -> throwError $ Fail err
instance Instantiate Constraint where
  instantiate (t `T.Implements` p) = instantiate $ t `T.Implements` p



generalize :: InferenceEnv -> [ImplConstraint] -> Type -> TypeExpr
-- generalize env impls t
--   | trace
--       ( "Generalizing: "
--           ++ show t
--           ++ "\n\tImplementation constraints: "
--           ++ show impls
--           ++ "\n\n\tFTV ty: "
--           ++ show (ftv t)
--           ++ "\n\tFTV env: "
--           ++ show (ftv env)
--       )
--       False =
--       undefined
generalize env impls t
  | Set.size tvars == 0 && null impls = TAtom t
  | Set.size tvars == 0               = TQualified $ fmap mkConstraint impls :=> TAtom t
  | otherwise                         = TQualified $ fmap mkConstraint impls :=> TLambda params (TAtom t)
  where
    tvars = ftv t
    params = Set.toList tvars <&> \(Tyvar v _) -> v
    mkConstraint (ty `IP` p) = ty `T.Implements` p


lookupEnv :: String -> Infer Type
lookupEnv x = do
  Saga {types} <- ask
  IST { unification } <- get
  case Map.lookup x types <|> lookup' x unification of
    Just tyExpr -> instantiate tyExpr
    Nothing     -> throwError $ UnboundVariable (show x)

  where
    lookup' x = fmap (TAtom . TVar) . Map.lookup x

infer :: Expr -> Infer Type
-- infer ex | trace ("Inferring: " ++ show ex) False = undefined
infer ex = case ex of
  Identifier x -> lookupEnv x

  Lambda (param : rest) body -> do
    tVar <- fresh KType
    out' <- infer out `scoped` (param, Tyvar param KType)
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

    let inferred = genArg `TArrow` out
    emit $ EqCons $ fnTy `EQ` inferred

    return out
  FnApp fn (a : as) -> infer curried
    where
      partial = FnApp fn [a]
      curried = foldl (\f a -> FnApp f [a]) partial as

  Match cond cases -> do
    cond' <- infer cond
    cases <- mapM inferCase cases
    emit $ EqCons $ cond' `EQ` TPrimitive TBool
    return $ TUnion cases
    where
      inferCase:: Case -> Infer Type
      inferCase (Case _ e) = infer e

  Tuple elems -> do
    tElems <- mapM infer elems
    return $ TTuple tElems
  Record pairs -> do
    tPairs <- mapM infer' pairs
    return $ TRecord tPairs
    where
      infer' = mapM infer
  List [] -> do
    var <- fresh KType
    return $ TApplied listConstructor var
  List elems -> do
    tys <- mapM infer elems
    let ty = head tys
    if all (ty ==) tys then
      return $ TApplied listConstructor ty
    else throwError $ Fail "Inferred different element types in a List"

  Block stmts -> infer' stmts
    where

      infer' [] = return TVoid
      infer' (stmt : rest) = case stmt of
        Return expr                   -> infer expr
        Declaration (Let id _ _ expr) -> do
          tVar <- fresh KType
          infer' rest `scoped` (id, Tyvar id KType)
        _ -> infer' rest

      inferStmt (Return expr)                        = infer expr
      inferStmt (Procedure expr)                     = infer expr
      inferStmt (Declaration (Let id tyExpr _ expr)) = infer expr
      inferStmt (Declaration _)                      = return TVoid
      tvars (TVar v) = [v]


  Literal literal -> return $ TLiteral literal
  ty -> error $ "Inference not implemented yet: " ++ show ty

generalizeArg :: Type -> Infer Type
generalizeArg (TLiteral lit) = generalize' $ case lit of
    LString _ -> ("IsString", TString)
    LBool _   -> ("IsBool", TBool)
    LInt _    -> ("Num", TInt)
    where
      generalize' (protocol, t) = do
        tVar <- fresh KType
        emit $ ImplCons $ tVar `IP` protocol
        return tVar

generalizeArg ty = return ty

normalize :: TypeExpr -> TypeExpr
-- normalize sc | trace ("\n\nNormalizing: " ++ show sc) False = undefined
normalize tyExpr = case tyExpr of
  (TQualified (cs :=> tyExpr')) -> TQualified $ (normConstraint <$> cs) :=> normalize tyExpr'
  (TAtom ty)                    -> TAtom $ normType ty
  (TLambda params tyExpr')       -> TLambda (fmap norm' params) (normalize tyExpr')
  (TTagged tag tyExpr')          -> TTagged tag $ normalize tyExpr'
  _ -> tyExpr

  where
    ord = zip tvars letters
    tvars = nub (Set.toList $ ftv tyExpr) <&> \(Tyvar v _) -> v
    norm' x = fromMaybe x (lookup x ord)

    normType (TTuple tys)        = TTuple $ fmap normType tys
    normType (TUnion tys)        = TUnion $ fmap normType tys
    normType (TRecord pairs)     = TRecord $ fmap (fmap normType) pairs
    normType (TData cons)        = TData $ normCons cons
    normType (TApplied cons arg) = normType cons `TApplied` normType arg
    normType (a `TArrow` b)      = normType a `TArrow` normType b
    normType (TVar (Tyvar v k))  = TVar $ Tyvar (norm' v) k
    normType t                   = t

    normConstraint (t `T.Implements` p) = normType t `T.Implements` p

    normCons (Tycon x k) = Tycon (norm' x) k



initState :: InferState
initState = IST {tvars = 0, kvars = 0, unification = Map.empty }

emit :: IConstraint -> Infer ()
emit = tell . pure

fresh :: Kind -> Infer Type
fresh k = do
  modify $ \s -> s {tvars = tvars s + 1}
  s <- get
  let v = "t" ++ show ([1 ..] !! tvars s)
  return $ TVar $ Tyvar v k

freshKind :: Infer Kind
freshKind = do
  modify $ \s -> s {kvars = kvars s + 1}
  s <- get
  let v = "k" ++ show ([1 ..] !! kvars s)
  return $ KVar v

scoped :: Infer a -> (String, Tyvar) -> Infer a
scoped m (id, tvar) = do
  modify $ \s -> s{ unification = Map.insert id tvar $ unification s  }
  m


