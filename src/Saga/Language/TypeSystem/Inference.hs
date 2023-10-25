{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TupleSections         #-}


module Saga.Language.TypeSystem.Inference where

import           Control.Applicative                    ((<|>))
import           Control.Monad.Except
import           Control.Monad.RWS

import           Control.Monad.Trans.Except             (ExceptT, runExceptT)
import qualified Control.Monad.Writer                   as W
import           Data.Bifunctor                         (Bifunctor (first))
import           Data.Functor                           ((<&>))
import           Data.List                              (intercalate, nub,
                                                         partition, (\\))
import qualified Data.Map                               as Map
import           Data.Maybe                             (fromMaybe, isJust,
                                                         isNothing)
import qualified Data.Set                               as Set
import           Debug.Trace                            (trace, traceM)
import           Prelude                                hiding (EQ, id, log)
import           Saga.Language.Core.Expr
import           Saga.Language.Core.Literals            (Literal (..))
import           Saga.Language.TypeSystem.Constraints   hiding (simplify,
                                                         unification)
import           Saga.Language.TypeSystem.Environment
import qualified Saga.Language.TypeSystem.Types         as T
import           Saga.Language.TypeSystem.Types         hiding (Implements)

import           Saga.Language.TypeSystem.Shared
import           Saga.Parser.Desugar
import           Saga.Parser.Parser                     (runSagaExpr)
import           Saga.Parser.Shared                     hiding (Record, Term,
                                                         Tuple, return)

import           Control.Monad.Identity                 (Identity)
import           Control.Monad.Reader                   (ReaderT (runReaderT))

import           Control.Monad.Writer
import           Data.Either                            (isLeft)
import           Data.Foldable                          (for_)
import           Saga.Language.TypeSystem.Constraints   (unify)
import           Saga.Language.TypeSystem.Errors        (SagaError (..))
import           Saga.Language.TypeSystem.Lib           (defaultEnv,
                                                         listConstructor)
import           Saga.Language.TypeSystem.Normalization
import qualified Saga.Language.TypeSystem.Refinement    as Refine
import           Saga.Language.TypeSystem.Types         (Constraint (Implements))
import           Saga.Utils.Utils                       (Pretty (pretty), (|>),
                                                         (||>))


--type Infer a = Saga IState (WriterT [IConstraint] (Except SagaError)) a

data Trace = Traced Accumulator [IConstraint] deriving (Show)
instance Semigroup Trace where
  (Traced acc1 cs1) <> (Traced acc2 cs2) = Traced (acc1 <> acc2) (cs1 <> cs2)
instance Monoid Trace where
  mempty = Traced mempty []

type Bindings = Map.Map String Type



-- inference :: Expr -> CompilerState -> IState -> m (TypeExpr, Map.Map String Type, Expr)
-- inference :: ( MonadWriter Trace (t (Except SagaError))
--   , MonadError SagaError (t (Except SagaError))
--   , MonadTrans t
--   ) => Expr -> CompilerState -> IState -> t (Except SagaError) (TypeExpr, Map.Map String Type, Expr)
--inference :: InferM Trace m => Expr -> CompilerState -> IState -> Either SagaError (TypeExpr, Map.Map String Type, Expr)
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


lookupEnv :: InferM Trace m => String -> m Type
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
inferDec d | trace ("\nInferring declaration:\n\t" ++ show d) False = undefined
inferDec d@(Type id (Just (KProtocol k)) spec') = do
  modify (\e -> e{ protocols = protocol : protocols e })
  return d
  where
    protocol = case spec' of
      TQualified (cs :=> spec) -> Protocol { id, spec, supers = cs <&> \(Implements _ p) -> p, implementations = []}
      spec                     -> Protocol { id, spec, supers = [], implementations = []}

inferDec d@(Type id _ typeExp) = do
  modify (\e -> e{ types = Map.insert id typeExp $ types e })
  return d

inferDec (Let id (Just ty) k expr) = do
  modify (\e -> e{ types = Map.insert id ty $ types e })
  env <- get
  case Refine.runIn env ty of
    Left err -> throwError $ Fail err
    Right refined -> do
      ((ty', _, expr'), Traced acc _ ) <- lift $ runWriterT $ inference (Typed expr refined) env initState
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

inferDec dec@(Data id k (TClause (TLambda params tyExpr) bindings)) = do

  modify (\e -> e{ kinds = Map.insert id kind $ kinds e })
  modify (\e -> e{ dataTypes = Map.insert id dataType $ dataTypes e })

  forM_ constructorFns $ \(tag, DCons { constructor  }) -> do
    traceM $ "Constructor type\n\t" ++ tag ++ "\n\t" ++ show constructor
    checkReturnType constructor
    -- | TODO:#namespaces This can be removed when introducing namespaces as we'll know to look for constructors in dataTypes
    modify (\e -> e{ types = Map.insert tag constructor $ types e })
  return dec

  where

    dataType = DataType userType (Map.fromList constructorFns)
    userType = TAtom $ TData (Tycon id kind)
    kind = foldr (KArrow . KVar) KType params
    constructorFns = members tyExpr

    members t@(TTagged tag cons)        = [(tag, constructor t)]
    members (TComposite (TEUnion tys) ) = tys <&> \case
        t@(TTagged tag cons)        -> (tag, constructor t)
        _ -> error $ "Union members of data type " ++ id ++ " must be tagged!"

    constructor (TTagged tag cons) = DCons (TLambda params cons) (memberData cons)

    memberData t = TLambda params $ TComposite $ TETuple $ init $ buildType t

    buildType (TComposite (TEArrow t1 t2)) = buildType t1 ++ buildType t2
    buildType t                            = [t]


    checkReturnType memberExpr    = do
      env <- get
      let extended = env { types =  Map.fromList args `Map.union` types env  }
      let ret = returnType memberExpr
      case check ret appliedType extended of
        Left err  -> throwError $ Fail err
        Right (sub, _, _) -> do
          traceM "\nFor type"
          traceM $ "\t" ++ show memberExpr
          traceM "Return type is:"
          traceM $ "\t" ++ show ret
          return (sub, ret)

      where
        appliedType = TFnApp userType (fmap snd args)
        args = params <&> \p -> (p, TAtom $ TVar $ Tyvar p $ KVar p)
        check t1 t2 env = do

          traceM $ "\nArgs:\n\t" ++ show args
          traceM $ "\nExtended:\n\t" ++ show (Map.keys $ types env)
          t1' <- Refine.runIn env t1
          t2' <- Refine.runIn env t2
          show `first` runExcept (runSolve env [EqCons $ t1' `EQ` t2'])








infer :: InferM Trace m => Expr -> m Expr
-- infer ex | trace ("Inferring: " ++ show ex) False = undefined
infer = infer_ 0

  where
    infer_ n ex = do
      let ident = intercalate "" (replicate n "\t")
      traceM $  ident ++ "Inferring: " ++ show ex
      result <- doInfer ex $ n+1
      --traceM $ "\nFor:\n\t"++ show ex
      traceM $ ident ++ "Result: " ++ show result
      return result

    extract (Typed _ ty) = ty

    doInfer :: InferM Trace m => Expr -> Int-> m Expr
    doInfer e n = case e of
      typed@(Typed expr ty) -> do
        Typed expr' inferred <- infer_ n expr
          -- | Inferred type is generalized as much as possible and unification expects the LHS to be the subtype
        case expr' of
          -- | When expression is a literal, it doesn't get generalized to preserve the literal type
          Literal _ -> emit $ EqCons $ inferred `EQ` ty
          -- | In any other case, we want to unify by instantiating the inferred type to the src type
          _         -> emit $ EqCons $ ty `EQ` inferred
        return typed
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

      FnApp dot@(Identifier ".") args@[recordExpr, Identifier field] -> do
        fieldType <- TVar <$> fresh KType
        (Typed _ recordTy) <- infer_ n recordExpr
        emit $ EqCons $ recordTy `EQ` TRecord [(field, fieldType)]
        return $ Typed (FnApp dot args) fieldType

      --FnApp dot@(Identifier ".") args -> throwError $ Fail $ "Unrecognised expressions for property access:\n\t" ++ show args
      FnApp (Identifier ".") args -> throwError $ Fail $ "Unrecognised expressions for property access:\n\t" ++ show args

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

          inferPat :: InferM Trace m => Pattern -> m (Writer [(String, Tyvar)] Type)
          inferPat Wildcard = do
            tVar <- TVar <$> fresh KType
            return $ return tVar


          inferPat (Id id) = do
            tVar <- fresh KType
            return $ W.writer (TVar tVar, [(id, tVar)])

          inferPat (Lit l) = return $ return (TLiteral l)

          inferPat (PatData tag pats) = do
            env@(Saga { types, dataTypes }) <- ask

            let constructors = Map.toList $ Map.mapMaybeWithKey (\k (DataType { members, userType }) -> Map.lookup tag members ) dataTypes
            case constructors of
              [(dataType, DCons { constructor, cdata })] -> do
                inferred <- sequence <$> mapM inferPat pats
                let (args, w) = runWriter inferred

                (dat, out) <- inst' env constructor cdata
                emit $ EqCons $ TTuple args `EQ` dat
                return $ W.writer (out, w)

              [] -> throwError $ Fail $ "Tag " ++ tag ++ " is not a constructor"
              multiple -> throwError $ Fail $ "Tag " ++ tag ++ " is a constructor for multiple data types: " ++ show multiple


            where
              base = TData (Tycon tag (KVar tag))

              inst' env cons dat = do
                dat'  <- instantiate dat
                out   <- returnType <$> instantiate cons
                return (dat', out)


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

generalize :: InferM Trace m => Type -> m Type
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




initState :: IState
initState = IST {tvars = 0, kvars = 0, unification = Map.empty, kUnification = Map.empty }

emit :: InferM Trace m => IConstraint -> m ()
emit c = tell $ Traced mempty (pure c)

fresh :: InferM Trace m => Kind -> m Tyvar
fresh k = do
  modify $ \s -> s {tvars = tvars s + 1}
  s <- get
  let v = "t" ++ show ([1 ..] !! tvars s)
  return $ Tyvar v k

freshKind :: InferM Trace m => m Kind
freshKind = do
  modify $ \s -> s {kvars = kvars s + 1}
  s <- get
  let v = "k" ++ show ([1 ..] !! kvars s)
  return $ KVar v

extended :: InferM Trace m => m a -> (String, Tyvar) -> m a
extended m (id, tvar) = do
  modify $ \s -> s{ unification = Map.insert id tvar $ unification s  }
  m

log :: MonadWriter Trace m => Log -> m ()
log str = tell $ Traced (Acc { logs = [str], warnings=[], errors = [] }) []




class ReturnType a where
  returnType :: a -> a

instance ReturnType TypeExpr where
  returnType (TAtom t)                   = TAtom $ returnType t
  returnType (TTagged _ ty)              = returnType ty
  returnType (TClause ty _)              = returnType ty
  returnType (TImplementation _ ty)      = returnType ty
  returnType (TQualified (_ :=> ty))     = returnType ty
  returnType (TComposite (TEArrow _ ty)) = returnType ty
  returnType (TLambda _ ty)              = returnType ty
  --returnType (TFnApp ty _)               = returnType ty

  returnType tyExpr                      = tyExpr

instance ReturnType Type where
  returnType (TArrow _ t) = returnType t
  returnType t            = t


class Instantiate a where
  instantiate :: InferM Trace m => a -> m Type



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
    --traceM $ "\nSubbed type expression:\t" ++ show te'
    tell $ Traced mempty $ mkIConstraint <$> apply sub cs

    instantiate te'
    where
      getKind (Tyvar v k) = k
      vars = Set.toList $ ftv cs

  instantiate (TLambda params tyExpr) = do
    env <- ask
    let extended = env { types =  Map.fromList args `Map.union` types env  }
    case Refine.runIn extended tyExpr of
      Right (TClosure _ body _ ) -> local (const extended) $ instantiate body
      Right ty                   -> return ty
      Left err                   -> throwError $ Fail err
    where
      args = params <&> \p -> (p, TAtom $ TVar $ Tyvar p $ KVar p)
  instantiate tyExpr = do
    env <- ask
    case Refine.runIn env tyExpr of
      Right (TClosure _ body _ ) -> instantiate body
      Right ty                   -> return ty
      Left err                   -> throwError $ Fail err
instance Instantiate Constraint where
  instantiate (t `T.Implements` p) = instantiate $ t `T.Implements` p
