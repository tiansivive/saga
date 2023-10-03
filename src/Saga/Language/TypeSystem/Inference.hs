{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TupleSections         #-}


module Saga.Language.TypeSystem.Inference where

import           Control.Applicative                  ((<|>))
import           Control.Monad.Except
import           Control.Monad.RWS                    (MonadReader (ask),
                                                       MonadWriter (tell),
                                                       RWST (runRWST), evalRWST,
                                                       execRWST, gets, modify')
import           Control.Monad.State.Lazy             (MonadState, State,
                                                       evalState, evalStateT,
                                                       replicateM)
import           Control.Monad.Trans.Except           (ExceptT, runExceptT)
import qualified Control.Monad.Writer                 as W
import           Data.Bifunctor                       (Bifunctor (first))
import           Data.Functor                         ((<&>))
import           Data.List                            (intercalate, nub,
                                                       partition, (\\))
import qualified Data.Map                             as Map
import           Data.Maybe                           (fromMaybe, isJust,
                                                       isNothing)
import qualified Data.Set                             as Set
import           Debug.Trace                          (trace, traceM)
import           Prelude                              hiding (EQ, log)
import           Saga.Language.Core.Literals          (Literal (..))
import           Saga.Language.Core.Syntax
import           Saga.Language.TypeSystem.Constraints hiding (simplify,
                                                       unification)
import           Saga.Language.TypeSystem.Environment
import qualified Saga.Language.TypeSystem.Types       as T
import           Saga.Language.TypeSystem.Types       hiding (Implements)

import           Saga.Language.TypeSystem.Shared
import           Saga.Parser.Desugar
import           Saga.Parser.Parser                   (runSagaExpr)
import           Saga.Parser.Shared                   hiding (Record, Term,
                                                       Tuple, return)

import           Control.Monad.Identity               (Identity)
import           Control.Monad.Reader                 (ReaderT (runReaderT))
import           Control.Monad.Trans.RWS              (get, local, modify)
import           Control.Monad.Writer
import           Data.Either                          (isLeft)
import           Saga.Language.TypeSystem.Errors      (SagaError (..))
import           Saga.Language.TypeSystem.Lib         (defaultEnv,
                                                       listConstructor)
import qualified Saga.Language.TypeSystem.Refinement  as Refine
import           Saga.Utils.Utils                     (Pretty (pretty), (|>),
                                                       (||>))


--type Infer a = Saga InferState (WriterT [IConstraint] (Except SagaError)) a
type Infer = RWST CompilerState Trace InferState (Except SagaError)
data InferState = IST { tvars :: Int, kvars:: Int, unification:: Map.Map String Tyvar  } deriving (Show)

data Trace = Traced Accumulator [IConstraint] deriving (Show)

instance Semigroup Trace where
  (Traced acc1 cs1) <> (Traced acc2 cs2) = Traced (acc1 <> acc2) (cs1 <> cs2)
instance Monoid Trace where
  mempty = Traced mempty []



run :: String -> Either String (TypeExpr, Bindings, Expr)
run input = do
  Parsed expr _ _ <- runSagaExpr input
  show `first` runExcept (runInfer defaultEnv (infer $ desugarExpr expr))

type Bindings = Map.Map String Type

runInfer :: CompilerState -> Infer Expr -> Except SagaError (TypeExpr, Bindings, Expr)
runInfer env m = do
  traceM "\n\n"
  traceM "--------------------"
  traceM "RUNNING INFERENCE"
  traceM "\n\n"
  (e, st, Traced acc constraints) <- runRWST m env initState

  case e of
    Typed e' ty -> do
      traceM "\n\n"
      traceM "--------------------"
      traceM "SOLVING CONSTRAINTS"
      traceM $ "Inferred Type:\n\t" ++ show ty
      traceM $ "\nInference state:\n\t" ++ show st
      --traceM $ "\nAccumulated logs: " ++ show acc
      -- traceM $ "\nEmitted Constraints:\n\t" ++ show constraints

      (subst, implConstraints, cycles) <- runSolve env constraints
      traceM "\n\n"
      traceM "--------------------"
      traceM "PROCESSING CYCLES"
      traceM $ "Cycles:\n\t" ++ show cycles
      traceM $ "\nSubst:\n\t" ++ pretty subst
      subst' <- resolveCycles subst cycles

      traceM "\n"
      traceM "--------------------"
      traceM "CLOSING OVER"
      traceM $ "\nFinal Subst:\n\t" ++ pretty subst'
      traceM $ "\nImpl constraints" ++ show implConstraints
      traceM "\n"
      let bindings = normalize Set.empty $ apply subst (TVar <$> unification st)
      let final = ty ||> apply subst' |> simplify
      traceM $ "\nFinal type:\n\t" ++ show final
      traceM $ "\nWith bindings:\n\t" ++ show bindings

      return (closeOver implConstraints final, bindings, normalize (ftv final) (apply subst' e))

    _ -> throwError $ Fail $ "Could not infer type for: " ++ show e


-- inference :: Expr -> CompilerState -> InferState -> m (TypeExpr, Map.Map String Type, Expr)
inference ::
  ( MonadWriter Trace (t (ExceptT SagaError Identity))
  , MonadError SagaError (t (ExceptT SagaError Identity))
  , MonadTrans t
  ) => Expr -> CompilerState -> InferState -> t (Except SagaError) (TypeExpr, Map.Map String Type, Expr)
inference expr env st = case runExcept $ runRWST (infer expr) env st of
  Left err -> do
    tell $ Traced (Acc { logs = [], warnings=[], errors = [err] }) []
    throwError err

  Right (Typed e ty, st, Traced acc cs) -> lift $ do
    (subst, implConstraints, cycles) <- runSolve env cs
    subst' <- resolveCycles subst cycles
    let bindings = normalize Set.empty $ apply subst (TVar <$> unification st)
    let final = ty ||> apply subst' |> simplify
    return (closeOver implConstraints final, bindings, normalize (ftv final) (apply subst' e))


resolveCycles :: MonadError SagaError m => Subst -> [Cycle] -> m Subst
resolveCycles = foldM collapse

-- | TODO: Check if any of the tvars are constrained. If so, then try to unify within the union. If it fails, it's an actual error
unbound :: Tyvar -> Subst -> Bool
unbound tvar subst = case Map.lookup tvar subst of
  Just (TVar tvar) -> unbound tvar subst
  Just _           -> False
  Nothing          -> True

collapse :: MonadError SagaError m => Subst -> Cycle -> m Subst
collapse sub (tvar, ty, solution) =
      if unbound tvar sub then
        return $ compose solution sub
      else throwError $ InfiniteType tvar ty

closeOver ::  [ImplConstraint] -> Type -> TypeExpr
closeOver cs ty = normalize (ftv tyExpr) tyExpr
  where
    tyExpr = qualify cs ty

qualify :: [ImplConstraint] -> Type -> TypeExpr
qualify impls t
  | Set.size tvars == 0 && null impls = TAtom t
  | Set.size tvars == 0               = TQualified $ fmap mkConstraint impls :=> TAtom t
  | otherwise                         = TQualified $ fmap mkConstraint impls :=> TLambda params (TAtom t)
  where
    tvars = ftv t
    params = Set.toList tvars <&> \(Tyvar v _) -> v
    mkConstraint (ty `IP` p) = ty `T.Implements` p


lookupEnv :: String -> Infer Type
lookupEnv x = do
  Saga { types } <- ask
  IST { unification } <- get
  case Map.lookup x types <|> lookup' x unification of
    Just tyExpr -> instantiate tyExpr
    Nothing     -> throwError $ UnboundVariable (show x)

  where
    lookup' x = fmap (TAtom . TVar) . Map.lookup x


instance MonadFail Identity where
  fail = error



inferScript :: CompilerState -> Script -> Either SagaError ([Declaration], CompilerState, Accumulator)
inferScript env (Script decs) = runExcept $ runRWST (forM decs inferDec) () env

inferDec :: Declaration -> Saga () (Except SagaError) Declaration
inferDec d@(Type id _ typeExp) = do
  modify (\e -> e{ types = Map.insert id typeExp $ types e })
  return d

inferDec (Let id (Just ty) k expr) = do
  modify (\e -> e{ types = Map.insert id ty $ types e })
  env <- get
  ((_, _, expr'), Traced acc _ ) <- lift $ runWriterT $ inference expr env initState
  tell acc
  return $ Let id (Just ty) k expr'

inferDec (Let id Nothing k expr) = do
  env <- get
  ((ty, expr'), _, Traced acc _) <- lift $ runRWST inference' env initState
  modify (\e -> e{ types = Map.insert id ty $ types e })
  tell acc
  return $ Let id (Just ty) k expr'

  where
    inference' = do
      tvar <- fresh KType
      st <- gets $ \s -> (s{ unification = Map.insert id tvar $ unification s })
      env <- ask
      (ty, bindings, expr') <- inference expr env st
      return (ty, expr')



infer :: Expr -> Infer Expr
-- infer ex | trace ("Inferring: " ++ show ex) False = undefined
infer = infer_ 0

  where
    infer_ n ex = do
      let ident = intercalate "" (replicate n "\t")
      traceM $  ident ++ "Inferring: " ++ show ex
      result <- doInfer ex $ n+1
      --traceM $ "For:\n\t"++ show ex
      traceM $ ident ++ "Result: " ++ show result
      return result

    extract (Typed _ ty) = ty

    doInfer :: Expr -> Int-> Infer Expr
    doInfer e n = case e of
      typed@(Typed {}) -> return typed
      Identifier x -> Typed e <$> lookupEnv x

      Lambda ps@(param : rest) body -> do
        tVar <- fresh KType
        t@(Typed _ out') <- infer_ n out `extended` (param, tVar)
        let ty = TVar tVar `TArrow` out'
        let expr = Lambda ps t
        return $ Typed expr ty
        where
          tvars (TVar v) = [v]
          out = case rest of
            [] -> body
            _  -> Lambda rest body
      FnApp fn [arg] -> do
        out <- TVar <$> fresh KType
        fn'@(Typed _ fnTy)   <- infer_ n fn
        arg'@(Typed _ argTy) <- infer_ n arg

        inferred <- generalize $ argTy `TArrow` out
        emit $ EqCons $ fnTy `EQ` inferred

        return $ Typed (FnApp fn' [arg']) out
      FnApp fn (a : as) -> infer_ n curried
        where
          partial = FnApp fn [a]
          curried = foldl (\f a -> FnApp f [a]) partial as

      Match cond cases -> do
        cond'@(Typed _ ty) <- condition
        cases' <- mapM inferCase cases
        let (tvars, tys) = foldl separate ([], []) cases'
        ty' <- mapM generalize $ case length tys of
              0 -> Nothing
              1 -> Just $ head tys -- | TODO: do we need this anymore? we now collapse unions...
              _ -> Just $ TUnion $ Set.fromList tys

        maybe (return ()) (emit . EqCons . EQ ty) ty'
        forM_ tvars $ \v -> emit $ EqCons $ v `EQ` fromMaybe ty ty'

        let out = TUnion . Set.fromList $ fmap extractTy cases'
        return $ Typed (Match cond' cases') out
        where
          extractTy (TypedCase _ _ (Typed _ ty)) = ty
          separate (tvars, tys) caseExpr = case caseExpr of
            TypedCase _ ty@(TVar _) _ -> (ty:tvars, tys)
            TypedCase _ ty _          -> (tvars, ty:tys)
            _                         -> error $ "Expected to see a typed case: " ++ show caseExpr

          condition = infer_ n cond
          --inferCase:: Case -> Infer (Type, Type)
          inferCase (Case pat expr) = do
            (patTy, tvars) <- runWriter <$> inferPat pat
            modify $ \s -> s{ unification = Map.fromList tvars `Map.union` unification s  }
            TypedCase pat patTy <$> infer_ n expr

          inferPat :: Pattern -> Infer (Writer [(String, Tyvar)] Type )
          inferPat Wildcard = do
            tVar <- TVar <$> fresh KType
            return $ return tVar


          inferPat (Id id) = do
            tVar <- fresh KType
            return $ W.writer (TVar tVar, [(id, tVar)])

          inferPat (Lit l) = return $ return (TLiteral l)

          inferPat (PatData tag pats) = do
            inferred <- sequence <$> mapM inferPat pats
            return $ foldl TApplied base <$> inferred
            where
              base = TData (Tycon tag KType)

          inferPat (PatTuple pats rest) = do
            inferred <- sequence <$> mapM inferPat pats
            return $ TTuple <$> inferred

          inferPat (PatList pats rest) = do
            inferred <- sequence <$> mapM inferPat pats
            tvar <- fresh KType
            let (tys, tvars) = runWriter inferred
            let result = TApplied listConstructor (ty (TVar tvar) tys)

            case rest of
              Nothing -> return $ W.writer (result, tvars)
              Just id -> do
                tvarList <- fresh KType
                emit $ EqCons $ result `EQ` TVar tvarList
                return $ W.writer (result, (id,  tvarList) : tvars)

            where
              ty tvar []     = tvar
              ty tvar (t:ts) = t

          inferPat (PatRecord pairs rest) = do
            inferred <- sequence <$> mapM inferPair pairs
            let result = TRecord <$> inferred
            case rest of
              Nothing -> return result
              Just id -> do
                tvarRecord' <- fresh KType
                return $ result >>= \t -> W.writer (t, [(id,  tvarRecord')])

            where
              inferPair p@(id, _) = fmap (id,) <$> inferPair' p
              inferPair' (id, Nothing) = do
                tvar <- fresh KType
                return $ W.writer (TVar tvar, [(id, tvar)])
              inferPair' (id, Just pat) = inferPat pat

      Tuple elems -> do
        elems' <- mapM (infer_ n) elems
        return $ Typed (Tuple elems') (TTuple $ fmap extract elems')

      Record pairs -> do
        pairs' <- mapM infer' pairs

        return $ Typed (Record pairs') (TRecord $ fmap extract <$> pairs')
        where
          infer' = mapM (infer_ n)
      e@(List []) -> do
        var <- TVar <$> fresh KType
        return $ Typed e (TApplied listConstructor var)
      List elems -> do
        elems' <- mapM (infer_ n) elems
        env <- ask
        let tys = fmap extract elems'
        ty <- generalize $ head tys
        let unification = (foldM (\sub t2 -> compose sub <$> unify ty t2 ) nullSubst tys :: Solve Subst)
        case runExcept . runWriterT $ runReaderT unification env of
          Left err -> throwError $ Fail $ show err ++ "\nInferred different element types in a List"
          Right _ -> return $ Typed (List elems') (TApplied listConstructor ty)
        -- if all (ty ==) tys then
        --   return $ Typed (List elems') (TApplied listConstructor ty)
        -- else throwError $ Fail "Inferred different element types in a List"

      e@(Block stmts) -> infer' [] stmts
        where

          infer' processed [] = return $ Typed (Block $ reverse processed) returnTy
            where
              returnTy = case head processed of
                (Return (Typed e ty)) -> ty
                _                     -> TVoid
          infer' processed (stmt : rest) = case stmt of
            Return expr                   -> do
              typed <- infer_ n expr
              infer' (Return typed : processed) []

            d@(Declaration (Type id _ typeExp)) -> do
              let scoped = local (\e -> e{ types = Map.insert id typeExp $ types e })
              scoped $ infer' (d : processed) rest
            Declaration (Let id (Just ty) k expr) -> do
              let scoped = local (\e -> e{ types = Map.insert id ty $ types e })
              scoped $ do
                (Typed expr' _) <- infer_ n expr
                infer' (Declaration (Let id (Just ty) k expr') :processed) rest
            Declaration (Let id Nothing k expr) -> do
              tvar <- fresh KType
              (Typed expr' ty) <- infer_ n expr `extended` (id, tvar)
              emit $ EqCons $ TVar tvar `EQ` ty
              let scoped = local (\e -> e{ types = Map.insert id (TAtom ty) $ types e })
              scoped $ infer' (Declaration (Let id (Just $ TAtom ty) k expr') : processed) rest
            d -> infer' (d:processed) rest
      e@(Literal literal) -> return $ Typed e (TLiteral literal)
      e -> error $ "Inference not implemented yet: " ++ show e


simplify :: Type -> Type
simplify (TTuple tys) = TTuple $ fmap simplify tys
simplify (TRecord pairs) = TRecord $ fmap (fmap simplify) pairs
simplify (TArrow arg out) = TArrow (simplify arg) (simplify out)
simplify (TApplied cons arg) = TApplied (simplify cons) (simplify arg)
simplify (TUnion tys) | Set.size tys == 1 = simplify . head . Set.elems $ tys
simplify t@(TUnion {}) = simplified
  where
    simplified = case reduce t of
      [ty] -> ty
      tys  -> TUnion $ Set.fromList tys

    reduce = flatten |> fmap simplify |> nub
    flatten :: Type -> [Type]
    flatten ty = concat $ case ty of
      (TUnion tys) -> flatten <$> Set.elems tys
      ty           -> pure [ty]

simplify t = t

generalize :: Type -> Infer Type
generalize (TUnion tys) = TUnion . Set.fromList <$> mapM generalize (Set.toList tys)
generalize (TTuple tys) = TTuple <$> mapM generalize tys
generalize (TRecord pairs) = TRecord <$> mapM (mapM generalize) pairs
generalize (TArrow arg out) = TArrow <$> generalize arg <*> pure out
generalize ty = case ty of
  TLiteral lit -> generalize' $ case lit of
    LString _ -> "IsString"
    LBool _   -> "IsBool"
    LInt _    -> "Num"
  TPrimitive prim -> generalize' $ case prim of
    TInt    -> "Num"
    TString -> "IsString"
    TBool   -> "IsBool"
  _ -> return ty

  where
    generalize' protocol = do
      tVar <- TVar <$> fresh KType
      emit $ ImplCons $ tVar `IP` protocol
      return tVar



class Normalized a where
  normalize :: Set.Set UnificationVar -> a -> a

instance Normalized TypeExpr where
  normalize ftv tyExpr = case tyExpr of
    (TQualified (cs :=> tyExpr')) -> TQualified $ (normConstraint <$> cs) :=> normalize ftv tyExpr'
    (TAtom ty)                    -> TAtom $ normalize ftv ty
    (TLambda params tyExpr')       -> TLambda (fmap norm' params) (normalize ftv tyExpr')
    (TTagged tag tyExpr')          -> TTagged tag $ normalize ftv tyExpr'
    _ -> tyExpr

    where
      ord = zip tvars letters
      tvars = ftv ||> Set.toList |> fmap (\(Tyvar v _) -> v)
      norm' x = fromMaybe x (lookup x ord)

      normConstraint (t `T.Implements` p) = normalize ftv t `T.Implements` p


instance Normalized Type where
  normalize ftv = normalize'
    where
      mapping = zip tvars letters
      tvars = ftv ||> Set.toList |> fmap (\(Tyvar v _) -> v)
      norm' x = fromMaybe x (lookup x mapping)

      normalize' = \case
        (TTuple tys)        -> TTuple $ fmap normalize' tys
        (TUnion tys)        -> TUnion . Set.fromList $ fmap normalize' (Set.toList tys)
        (TRecord pairs)     -> TRecord $ fmap (fmap normalize') pairs
        (TApplied cons arg) -> normalize' cons `TApplied` normalize' arg
        (a `TArrow` b)      -> normalize' a `TArrow` normalize' b
        (TVar (Tyvar v k))  -> TVar $ Tyvar (norm' v) k
        (TData (Tycon x k)) -> TData $ Tycon (norm' x) k
        t                   -> t

instance Normalized (Map.Map String Type) where
  normalize _ unifier = fmap (normalize tvars) unifier
    where
      vals = snd <$> Map.toList unifier
      tvars = foldl (\vars t -> vars <> ftv t) Set.empty vals

instance Normalized Expr where
  normalize ftv = normalize'
    where
      mapping = zip tvars letters
      tvars = ftv ||> Set.toList |> fmap (\(Tyvar v _) -> v)
      norm' x = fromMaybe x (lookup x mapping)

      normalize' = \case
        Typed e ty -> Typed (normalize' e) (normalize ftv ty)

        Lambda ps body -> Lambda (fmap norm' ps) (normalize' body)
        FnApp fn args  -> FnApp (normalize' fn) (fmap normalize' args)
        Match cond cases -> Match (normalize' cond) (fmap (normalize ftv) cases)

        Record es -> Record $ fmap normalize' <$> es
        Tuple es  -> Tuple $ fmap normalize' es
        List es   -> List $ fmap normalize' es

        Block stmts -> Block $ fmap (normalize ftv) stmts
        e           -> e

instance Normalized Case where
  normalize ftv = \case
    Case pat e -> Case pat (normalize ftv e)
    TypedCase pat ty e -> TypedCase pat (normalize ftv ty) (normalize ftv e)

instance Normalized Statement where
  normalize ftv = \case
    Return e -> Return $ normalize ftv e
    Procedure e -> Procedure $ normalize ftv e
    Declaration d -> Declaration $ normalize ftv d

instance Normalized Declaration where
  normalize ftv = \case
    Let id ty k e -> Let id (fmap (normalize ftv) ty) k (normalize ftv e)
    Type id k ty -> Type id k (normalize ftv ty)
    Data id k dataExps binds -> Data id k (fmap (fmap $ normalize ftv) dataExps) binds



initState :: InferState
initState = IST {tvars = 0, kvars = 0, unification = Map.empty }

emit :: IConstraint -> Infer ()
emit c = tell $ Traced mempty (pure c)

fresh :: Kind -> Infer Tyvar
fresh k = do
  modify $ \s -> s {tvars = tvars s + 1}
  s <- get
  let v = "t" ++ show ([1 ..] !! tvars s)
  return $ Tyvar v k

freshKind :: Infer Kind
freshKind = do
  modify $ \s -> s {kvars = kvars s + 1}
  s <- get
  let v = "k" ++ show ([1 ..] !! kvars s)
  return $ KVar v

extended :: Infer a -> (String, Tyvar) -> Infer a
extended m (id, tvar) = do
  modify $ \s -> s{ unification = Map.insert id tvar $ unification s  }
  m


log :: MonadWriter Trace m => Log -> m ()
log str = tell $ Traced (Acc { logs = [str], warnings=[], errors = [] }) []

class Instantiate a where
  instantiate :: a -> Infer Type



instance Instantiate Type where

  instantiate (TClosure _ body _) = instantiate body
  instantiate (TApplied cons arg) = TApplied <$> instantiate cons <*> instantiate arg
  instantiate (TUnion tys) = TUnion . Set.fromList <$> mapM instantiate (Set.toList tys)
  instantiate (TTuple tys) = TTuple <$> mapM instantiate tys
  instantiate (TRecord pairs) = TRecord <$> mapM (mapM instantiate) pairs
  instantiate (TArrow inTy outTy) = TArrow <$> instantiate inTy <*> instantiate outTy
  instantiate ty = return ty

instance Instantiate TypeExpr where
  --instantiate te | trace ("\n\n------------\nInstantiating: " ++ show te) False = undefined
  instantiate (TQualified (cs :=> te)) = do
    tVars <- mapM (fmap TVar . fresh . getKind) vars
    let sub = Map.fromList $ zip vars tVars
    let te' = apply sub te

    -- traceM $ "\nZipped:\t" ++ show sub
    -- traceM $ "\nSubbed type expression:\t" ++ show te'
    tell $ Traced mempty $ mkIConstraint <$> apply sub cs

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
