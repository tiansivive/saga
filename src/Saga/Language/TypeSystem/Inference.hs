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
                                                       execRWST, modify')
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
import           Control.Monad.Trans.RWS              (get, local, modify)
import           Control.Monad.Writer
import           Saga.Language.TypeSystem.Errors      (SagaError (..))
import           Saga.Language.TypeSystem.Lib         (defaultEnv,
                                                       listConstructor)
import qualified Saga.Language.TypeSystem.Refinement  as Refine
import           Saga.Utils.Utils                     (Pretty (pretty), (|>))


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
      let final = simplify $ apply subst' ty
      let bindings = normalize $ apply subst (TVar <$> unification st)
      traceM $ "\nFinal type:\n\t" ++ show final
      traceM $ "\nWith bindings:\n\t" ++ show bindings

      return (closeOver implConstraints final, bindings, e)

    _ -> throwError $ Fail $ "Could not infer type for: " ++ show e


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
closeOver cs = normalize . qualify cs

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
  Saga { values } <- ask
  IST { unification } <- get
  case Map.lookup x values <|> lookup' x unification of
    Just tyExpr -> instantiate tyExpr
    Nothing     -> throwError $ UnboundVariable (show x)

  where
    lookup' x = fmap (TAtom . TVar) . Map.lookup x


instance MonadFail Identity where
  fail = error

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
        let tys = fmap extract elems'
        let ty = head tys
        if all (ty ==) tys then
          return $ Typed (List elems') (TApplied listConstructor ty)
        else throwError $ Fail "Inferred different element types in a List"

      e@(Block stmts) -> infer' [] stmts
        where

          infer' processed [] = return $ Typed (Block $ reverse processed) TVoid
          infer' processed (stmt : rest) = case stmt of
            Return expr                   -> do
              typed <- infer_ n expr
              infer' (Return typed : processed) []

            d@(Declaration (Type id _ typeExp)) -> do
              let scoped = local (\e -> e{ types = Map.insert id typeExp $ types e })
              scoped $ infer' (d : processed) rest
            Declaration (Let id (Just ty) k expr) -> do
              let scoped = local (\e -> e{ values = Map.insert id ty $ values e })
              scoped $ do
                (Typed expr' _) <- infer_ n expr
                infer' (Declaration (Let id (Just ty) k expr') :processed) rest
            Declaration (Let id Nothing k expr) -> do
              tvar <- fresh KType
              (Typed expr' ty) <- infer_ n expr `extended` (id, tvar)
              emit $ EqCons $ TVar tvar `EQ` ty
              let scoped = local (\e -> e{ values = Map.insert id (TAtom ty) $ values e })
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
  normalize :: a -> a

instance Normalized TypeExpr where
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
      normType (TUnion tys)        = TUnion . Set.fromList $ fmap normType (Set.toList tys)
      normType (TRecord pairs)     = TRecord $ fmap (fmap normType) pairs
      normType (TData cons)        = TData $ normCons cons
      normType (TApplied cons arg) = normType cons `TApplied` normType arg
      normType (a `TArrow` b)      = normType a `TArrow` normType b
      normType (TVar (Tyvar v k))  = TVar $ Tyvar (norm' v) k
      normType t                   = t

      normConstraint (t `T.Implements` p) = normType t `T.Implements` p

      normCons (Tycon x k) = Tycon (norm' x) k

instance Normalized (Map.Map String Type) where
  normalize unifier = fmap normType unifier
    where
      vals = snd <$> Map.toList unifier
      tvars = foldl (\vars t -> vars `Set.union` ftv t) Set.empty vals
      ord = zip (Set.toList tvars <&> \(Tyvar v _) -> v) letters

      norm' x = fromMaybe x (lookup x ord)

      normType (TTuple tys)        = TTuple $ fmap normType tys
      normType (TUnion tys)        = TUnion . Set.fromList $ fmap normType (Set.toList tys)
      normType (TRecord pairs)     = TRecord $ fmap (fmap normType) pairs
      normType (TData cons)        = TData $ normCons cons
      normType (TApplied cons arg) = normType cons `TApplied` normType arg
      normType (a `TArrow` b)      = normType a `TArrow` normType b
      normType (TVar (Tyvar v k))  = TVar $ Tyvar (norm' v) k
      normType t                   = t

      normCons (Tycon x k) = Tycon (norm' x) k



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
