{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE TupleSections     #-}

module Saga.Language.TypeSystem.Constraints where

import           Control.Monad.Except
import           Control.Monad.Reader                 (MonadReader (local),
                                                       ReaderT (runReaderT))
import           Control.Monad.RWS                    (MonadReader (ask), RWST)
import           Control.Monad.State                  (StateT (runStateT),
                                                       evalStateT, get, modify,
                                                       put)
import           Control.Monad.Trans.Writer           (WriterT (runWriterT))
import           Control.Monad.Writer                 (MonadWriter (tell))
import           Data.Bifunctor                       (bimap)
import           Data.Either                          (fromRight)
import           Data.Functor                         ((<&>))
import           Data.List                            (delete, find, groupBy,
                                                       intercalate, intersect,
                                                       partition, (\\))
import           Data.List.NonEmpty                   (groupWith)
import qualified Data.List.NonEmpty                   as NonEmpty
import qualified Data.Map                             as Map
import           Data.Maybe                           (catMaybes, fromJust,
                                                       fromMaybe, isJust)
import qualified Data.Set                             as Set
import           Debug.Trace
import           GHC.IO                               (unsafeDupablePerformIO)
import           Prelude                              hiding (EQ, id)
import           Saga.Language.Core.Literals          (Literal (..))
import           Saga.Language.Core.Syntax            (Case (..),
                                                       Declaration (..),
                                                       Expr (..),
                                                       Statement (..))
import           Saga.Language.TypeSystem.Environment hiding (Implements)
import           Saga.Language.TypeSystem.Errors      (SagaError (..))
import           Saga.Language.TypeSystem.Lib
import qualified Saga.Language.TypeSystem.Refinement  as Refine
import           Saga.Language.TypeSystem.Types       hiding (ProtocolID,
                                                       implementationTy)
import           Saga.Parser.ParsingInfo              (implementation,
                                                       tyBinding)
import           Saga.Utils.Utils
import           Text.Pretty.Simple                   (pPrintDarkBg, pShow)
import           Unsafe.Coerce                        (unsafeCoerce)
-- type Solve = StateT SolveState (Except InferenceError)

type Solve = ReaderT CompilerState (WriterT [Cycle] (Except SagaError))


type Subst = Map.Map Tyvar Type
type Cycle = (Tyvar, Type, Subst)

newtype SolveState = SST { unifier :: (Subst, [IConstraint]) }


-- runSolve :: CompilerState -> [IConstraint] -> Except SagaError (Subst, [ImplConstraint])
-- runSolve cs | trace ("Solving: " ++ show cs) False = undefined
runSolve :: CompilerState -> [IConstraint] -> Except SagaError (Subst, [ImplConstraint], [Cycle])
runSolve env cs = do
  ((sub, cs), cycles) <- runWriterT $ runReaderT (solver cs) env
  return (sub, cs, cycles)


printEqs eqs = "EQs:\n\t\t" ++ intercalate "\n\t\t" (eqs <&> \(EQ t1 t2) -> show t1 ++ " == " ++ show t2)

solver :: [IConstraint] -> Solve (Subst, [ImplConstraint])

solver constraints = do
  let eqs' = eqs constraints
  --traceM $ printEqs eqs'
  sub <- unification nullSubst eqs'
  --traceM $ "\nUnifier from equality constraints\n\t" ++ pretty sub
  -- traceM $ "\nGenerated unions\n\t" ++ show unionTvars
  --let sub' = Map.foldlWithKey toSubst sub unionTvars
  -- traceM $ "\nUnion TVar subst\n\t" ++ show sub'
  -- --sub'' <- checkUnions sub' $ apply sub' $ union constraints
  -- traceM $ "\nFinal subst\n\t" ++ pretty sub
  --traceM $ "\nMGU:\n\t" ++ show sub
  is <- reduce $ apply sub $ impls constraints
  resolve is
  return (sub, is)


  where
    impls  cs =  [ ip   | ImplCons ip   <- cs ]
    eqs    cs =  [ eq   | EqCons eq     <- cs ]
    union  cs =  [ uni  | UnionCons uni <- cs ]

    unionTvars = buildUnions (union constraints) Map.empty
    toSubst s k val = compose (Map.singleton k $ TUnion val) s



buildUnions :: [Union] -> Map.Map Tyvar [Type] -> Map.Map Tyvar [Type]
buildUnions [] unions = unions
buildUnions (t1 `MemberOf` TVar tvar : us) unions = buildUnions us $ Map.insertWith mappend tvar [t1] unions

checkUnions :: Subst -> [Union] -> Solve Subst
checkUnions sub []                                 = return sub
checkUnions sub (t1 `MemberOf` t2@(TUnion _) : us) = unify t1 t2 >>= \sub -> checkUnions sub us
checkUnions _   (t1 `MemberOf` t2 : us)            = throwError $ Fail $ "Type " ++ show t2 ++ " is not a Union type"


unification :: Subst -> [Equality] -> Solve Subst
--unification s cs | trace ("\nUnification:" ++ "\n\tUnifier:\n\t" ++ pretty s ++ "\n\t" ++ printEqs cs) False = undefined
unification s [] = return s
unification s (e:es) | t1 `EQ` t2 <- e = do
  sub <- unify t1 t2
  let sub' = compose sub s
  sub'' <- unification sub' $ apply sub' es
  return $ sub'' `compose` sub'




unify :: (MonadReader CompilerState m, MonadWriter [Cycle] m, MonadError e m, e ~ SagaError) => Type -> Type -> m Subst
--unify t1 t2 | trace ("Unifying:\n\t" ++ show t1 ++ "\n\t" ++ show t2) False = undefined
unify = unify_ 0
  where
    unify_ :: (MonadReader CompilerState m, MonadWriter [Cycle] m, MonadError e m, e ~ SagaError) => Int -> Type -> Type -> m Subst
    unify_ n t1 t2 = do
      let ident = intercalate "" (replicate n "\t")
      -- traceM (ident ++ "Unifying:\n" ++ ident ++ "  " ++ show t1 ++ "\n" ++ ident ++ "  " ++ show t2)
      result <- unify' t1 t2
      -- traceM (ident ++ "Result:\n" ++ ident ++ "  " ++ show result)
      return result
      where
        unify' :: (MonadReader CompilerState m, MonadWriter [Cycle] m, MonadError e m, e ~ SagaError) => Type -> Type -> m Subst
        unify' (TVar a) t = bind a t
        unify' t (TVar a) = bind a t
        unify' (TLiteral a) (TLiteral b) | a == b = return nullSubst
        unify' (TPrimitive a) (TPrimitive b) | a == b = return nullSubst
        unify' (TData lCons) (TData rCons) | lCons == rCons = return nullSubst
        unify' sub@(TRecord as) parent@(TRecord bs) = sub `isSubtype` parent
        unify' lit@(TLiteral _) prim@(TPrimitive _) = lit `isSubtype` prim
        unify' (TTuple as) (TTuple bs) = do
          ss <- zipWithM (unify_ $ n+1) as bs
          return $ foldl compose nullSubst ss

        unify' (il `TArrow` ol) (ir `TArrow` or) = do
          sub <- unify_ (n+1) il ir
          s <- unify_ (n+1) (apply sub ol) (apply sub or)
          return $ s `compose` sub
        unify' (TApplied f t) (TApplied f' t') = do
          sub <- unify_ (n+1) f f'
          s <-  unify_ (n+1) (apply sub t) (apply sub t')
          return $ s `compose` sub


        unify' u1@(TUnion tys1) u2@(TUnion tys2)  = do
          subs <- forM (Set.toList tys1) $ \t1 -> forM (Set.toList tys2) (unifier t1) <&> catMaybes
          --traceM $ ("\nAll subs:\n\t" ++ show subs)
          if not $ any null subs then
            return $ foldl (foldl compose) nullSubst subs
          else throwError $ UnificationFail u1 u2
          where
            unifier t1 t2 = catchError (unify_ (n+1) t1 t2 <&> Just) (\_ -> return Nothing)

        unify' t1@(TUnion tys) t2 = foldM unifier nullSubst (Set.toList tys)
          where
            unifier sub t = compose sub <$> unify_ (n+1) (apply sub t) t2
        unify' t1 t2@(TUnion tys) = do
          subs <- catMaybes <$> mapM (unifier t1) (Set.toList tys)
          if null subs then
            throwError $ UnificationFail t1 t2
          else return $ foldl compose nullSubst subs
          where
            unifier t1 t2 = catchError (unify_ (n+1) t1 t2 <&> Just) (\_ -> return Nothing)


        unify' t t' | kind t /= kind t' = throwError $ Fail "Kind mismatch"
        unify' t1 t2 = case (t1, t2) of
          (t@(TClosure {}), t2) -> do
            t' <- refine t
            unify_ (n+1) t' t2
          (t, t'@(TClosure {})) -> do
            t2' <- refine t2
            unify_ (n+1) t t2'
          _                     -> throwError $ UnificationFail t1 t2
          where
            refine :: (MonadReader CompilerState m, MonadError e m, e ~ SagaError) => Type -> m Type
            refine (TClosure params tyExpr captured) = do
              env <- ask
              -- traceM "\n\nRefine TClosure:"
              -- traceM $ "Env:\n\t" ++ pretty (types env)
              -- traceM $ "Extended:\n\t" ++ pretty (types $ extended env)
              -- traceM $ "Captured:\n\t" ++ pretty captured ++ "\n---------------------\n\n"
              case Refine.runIn (extended env) tyExpr of
                Left err -> throwError $ Fail $ "Failed to refine TClosure while unifying:\n\t" ++ err
                Right ty -> return ty
              where
                tvars = fmap (TAtom . TVar . (`Tyvar` KType)) params
                params' =  Map.fromList $ zip params tvars
                extended env = env{ types = params' `Map.union` captured `Map.union` types env }



bind :: (MonadReader CompilerState m, MonadWriter [Cycle] m, MonadError e m, e ~ SagaError) => Tyvar -> Type -> m Subst
--bind a t | trace ("Binding: " ++ show a ++ " to " ++ show t) False = undefined
bind a t
  | t == TVar a = return nullSubst
  | occursCheck a t = case t of
      TUnion tys | TVar a `elem` tys -> do
        let tys' = Set.delete (TVar a) tys
        let solution = Map.singleton a $ TUnion tys'
        tell [(a, t, solution)]
        return nullSubst
      _                              -> throwError $ InfiniteType a t
  | kind a /= kind t = throwError $ Fail "kinds do not match"
  | TLiteral l <- t = return . Map.singleton a $
      case l of
        LInt _    -> TPrimitive TInt
        LString _ -> TPrimitive TString
        LBool _   -> TPrimitive TBool
  | otherwise = return $ Map.singleton a t

occursCheck :: Substitutable a => Tyvar -> a -> Bool
occursCheck a t = a `Set.member` set
  where
    set = ftv t

isSubtype :: (MonadReader CompilerState m, MonadWriter [Cycle] m, MonadError e m, e ~ SagaError) => Type -> Type -> m Subst
-- isSubtype  a b | trace ("subtype " ++ show a ++ " <: " ++ show b ++ "\n  ") False = undefined
TLiteral (LInt _) `isSubtype` TPrimitive TInt = return nullSubst
TLiteral (LString _) `isSubtype` TPrimitive TString = return nullSubst
TLiteral (LBool _) `isSubtype` TPrimitive TBool = return nullSubst
TPrimitive prim1 `isSubtype` TPrimitive prim2 | prim1 == prim2 = return nullSubst
sub@(TRecord pairs1) `isSubtype` parent@(TRecord pairs2) = do
  let check (name, ty2) = case lookup name pairs1 of
        Nothing  -> throwError $ UnificationFail sub parent
        Just ty1 -> unify ty1 ty2

  subs <- mapM check pairs2
  return $ foldl compose nullSubst subs
sub `isSubtype` parent = throwError $ SubtypeFailure sub parent

nullSubst :: Subst
nullSubst = Map.empty


-- | Implementation Constraints solving
resolve :: [ImplConstraint] -> Solve ()
--resolve cs | trace ("\n\nProtocol Resolution:\n\t" ++ show cs) False = undefined
resolve constraints = do
  --traceM $ "\tGrouped Constraints: " ++ show constraintsPerType
  --traceM $ "\tGrouped Implementations: " ++ show (groupBy byType' (impls env))
  forM_ constraintsPerType (check allImplemented)


  where

    constraintsPerType = constraints ||> groupWith type' |> fmap pair |> Map.fromList
    pair group = (type' $ NonEmpty.head group, protocol' <$> NonEmpty.toList group)

    check f ids = do
      Saga { protocols } <- ask
      case f ids protocols of
        Nothing -> throwError $ Fail $ "Could not resolve for protocols: " ++ show ids
        Just _ -> return ()


    -- allImplemented ids ps | trace "\nImplements all:" False = undefined
    -- allImplemented ids ps | trace ("\tIds: " ++ show ids) False = undefined
    -- allImplemented ids ps | trace ("\tProtocols: " ++ show (fmap id ps)) False = undefined
    allImplemented ids env = do
      --traceM $ "Protocols found:\n\t" ++ show (fmap (fmap id) protocols')
      check <- implemented <$> sequence protocols'
      when check (return ())
     -- mapM (implementations |> find (matches ty)) foo

      where
        --implemented ps | trace ("\nImplementing types:\n\t" ++ show (foldl atLeastOneType [] ps)) False = undefined
        implemented ps = not $ null $ foldl atLeastOneType [] ps

        atLeastOneType [] p  = extract <$> implementations p
        atLeastOneType tys p = tys `intersect` (extract <$> implementations p)

        extract (_ :=> (t, _)) = t
        protocols' = ids <&> \id' -> env ||> find (\(Protocol { id }) -> id == id')


    type' (IP ty _)      = ty
    protocol' (IP _ p) = p



reduce :: [ImplConstraint] -> Solve [ImplConstraint]
--reduce cs | trace ("\nReducing\n\tImplementation constraint:" ++ show cs) False = undefined
reduce cs = mapM toHNF cs >>= simplify . concat

toHNF :: ImplConstraint -> Solve [ImplConstraint]
toHNF ip | inHNF ip   = return [ip]
         | otherwise =  do
            ipConstraints <- byImplementation ip
            ipConstraints' <- mapM toHNF ipConstraints
            return $ concat ipConstraints'

inHNF :: ImplConstraint -> Bool
inHNF (ty `IP` p) = hnf ty
 where hnf (TVar v) = True
       hnf _        = False

byImplementation :: ImplConstraint -> Solve [ImplConstraint]
--byImplementation impl | trace ("\nSearch by Implementation:\n\t" ++ show impl) False = undefined
byImplementation (ty `IP` p)    = do
  Saga { protocols } <- ask
  concat <$> sequence [ tryInst impl | impl <- impls p protocols ]

  where
    impls id' = find (\(Protocol { id }) -> id == id') |> maybe [] implementations
    tryInst (cs :=> (ty', e)) = do
      sub <- catchError (ty' `unify` ty) (handleErr |> throwError)
      return $ fmap (apply sub . mkIP) cs

    handleErr (UnificationFail t1 t2) = Fail $ "Types do not match:\n\t" ++ show t1 ++ "\n\t" ++ show t2
    handleErr err = err
    mkIP (ty `Implements` p) = ty `IP` p


simplify   :: [ImplConstraint] -> Solve [ImplConstraint]
-- simplify cs | trace ("\nSimplifying\n\tImplementation constraint:" ++ show cs) False = undefined
simplify = loop []
 where
  loop checked []     = return checked
  loop checked (ipc:ipcs) = do
    entailed <- entail (checked ++ ipcs) ipc
    if entailed
      then loop checked ipcs
      else loop (ipc: checked) ipcs

entail :: [ImplConstraint] -> ImplConstraint -> Solve Bool
-- entail ipcs current | trace ("\nEntailing\n\tCurrent: " ++ show current ++ "\n\tOthers:" ++ show ipcs) False = undefined
entail ipcs ipConstraint = do
  protocols <- ask
  baseConstraints <- mapM byBase ipcs
  --traceM $ "Checking by base constraints:\n\t" ++ show baseConstraints
  if any (ipConstraint `elem`) baseConstraints
    then return True
    else checkImpls

  where
    checkImpls = do
      constraints <- byImplementation ipConstraint
      --traceM $ "Checking by implementations:\n\t" ++ show constraints
      entailments <- mapM (entail ipcs) constraints
      --traceM $ "Entailments:\n\t" ++ show entailments
      return (not (null entailments) && and entailments)


byBase :: ImplConstraint -> Solve [ImplConstraint]
-- byBase impl | trace ("\nSearching base constraints of\n\t" ++ show impl) False = undefined
byBase impl@(ty `IP` p) = do
    Saga { protocols } <- ask
    impls <- sequence [ byBase (ty `IP` base) | base <- sups p protocols]
    let all = impl : concat impls
    --traceM $ "Found:\n\t" ++ show all
    return all
    where
      sups id' = maybe [] supers . find (\(Protocol { id }) -> id == id')








-- type Ambiguity = (TyVar, [IConstraint])

-- ambiguities :: [TyVar] -> [IConstraint] -> [Ambiguity]
-- ambiguities vars cs = do
--   v <- Set.toList (ftv cs) \\ vars
--   return (v, filter (elem v . ftv) cs)

-- candidates :: Ambiguity -> Solve [Type]
-- candidates (var, cs) = do
--   cs' <- filterM (entail []) cs
--   return $ implementationTy <$> cs'
--   where
--     ps =  [ p  | ty `Implements` p <- cs ]
--     tys = [ ty | ty `Implements` p <- cs ]
--     cs =  [ t' `Implements` p | all (TVar var ==) tys,
--                                 any (`elem` numProtocols) ps,
--                                 all (`elem` stdProtocols) ps,
--                                 p <- ps,
--                                 t' <- builtInTypes
--           ]

-- -- | Find the defaults for a combo of type vars and constraints
-- withDefaults :: ([Ambiguity] -> [Type] -> a) -> [TyVar] -> [IConstraint] -> Solve a
-- withDefaults f vars cs = do
--     tys' <- mapM candidates as
--     if any null tys'
--       then throwError $ Fail "cannot resolve ambiguity"
--       else return $ f as $ map head tys'

--     where
--       as = ambiguities vars cs

-- defaultedConstraints:: [TyVar] -> [IConstraint] -> Solve [IConstraint]
-- defaultedConstraints = withDefaults (\as _ -> concatMap snd as)

-- split :: [TyVar] -> [TyVar] -> [IConstraint] -> Solve ([IConstraint], [IConstraint])
-- split vars vars' cs = do
--   cs' <- reduce cs
--   let (deferred, retained) = partition (all (`elem` vars) . ftv) cs'
--   retained' <- defaultedConstraints (vars ++ vars') retained
--   return (deferred, retained \\ retained')



class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set.Set UnificationVar

instance (Substitutable a, Functor f, Foldable f) => Substitutable (f a) where
  apply = fmap . apply

  ftv = foldl union Set.empty
    where
      union set x = Set.union (ftv x) set


instance Substitutable Type where
  --apply s t | trace ("Applying type sub\n\t" ++ show s ++ "\n\t" ++ show t) False = undefined
  apply s t@(TVar id)                     = Map.findWithDefault t id s
  apply s (TTuple elems)                  = TTuple $ apply s <$> elems
  apply s (TRecord pairs)                 = TRecord $ apply s <$> pairs
  apply s (TUnion elems)                  = TUnion . Set.fromList $ apply s <$> Set.toList elems
  apply s (TApplied cons arg)             = TApplied (apply s cons) (apply s arg)
  apply s (TClosure params body env)      = TClosure params (apply s body) (apply s env)
  apply s (inTy `TArrow` outTy)           = in' `TArrow` out'
    where
      in' = apply s inTy
      out' = apply s outTy

  apply _ ty = ty

  ftv (TVar id)                = Set.singleton id
  ftv (TTuple elems)           = ftv elems
  ftv (TRecord pairs)          = ftv pairs
  ftv (TUnion tys)             = ftv (Set.toList tys)
  ftv (cons `TApplied` arg)    = ftv cons `Set.union` ftv arg
  ftv (t `TArrow` t')          = ftv t `Set.union` ftv t'
  ftv (TClosure params body _) = ftv body
  ftv _                        = Set.empty

instance Substitutable TypeExpr where
  --apply s t | trace ("Applying typeExpr sub: " ++ show s ++ " to " ++ show t) False = undefined
  apply s (TAtom ty)                      = TAtom $ apply s ty
  apply s (TComposite comp)               = TComposite $ apply s comp
  apply s (TTagged tag tyExpr)            = TTagged tag $ apply s tyExpr
  apply s (TClause tyExpr bindings)        = TClause (apply s tyExpr) (fmap apply' bindings)
    where
      apply' (ImplBind tyExpr prtcl) = ImplBind (apply s tyExpr) prtcl
      apply' b                       = b
  apply s (TImplementation prtcl tyExpr)  = TImplementation prtcl $ apply s tyExpr
  apply s (TFnApp fn args)                = TFnApp (apply s fn) (apply s args)
  apply s (TLambda params tyExpr)         = TLambda (subStrVar s <$> params) $ apply s tyExpr
    where
      subStrVar sub str = case Map.lookup (Tyvar str KType) sub of
        Nothing                   -> str
        Just (TVar (Tyvar id _ )) -> id
  apply s (TQualified (cs :=> ty)) = TQualified (cs' :=> ty')
    where
      ty' = apply s ty
      cs' = apply s <$> cs
  apply _ t  = t

  ftv (TAtom ty)                      = ftv ty
  ftv (TLambda _ tyExpr)              = ftv tyExpr
  ftv (TFnApp fn args)                = ftv fn `Set.union` args' where args' = Set.unions $ fmap ftv args
  ftv (TComposite comp)               = ftv comp
  ftv (TTagged tag tyExpr)            = ftv tyExpr
  ftv (TClause tyExpr bindings)       = foldl (\tvars b -> tvars `Set.union` ftv' b) (ftv tyExpr) bindings
    where
      ftv' (ImplBind tyExpr _)     = ftv tyExpr
      ftv' (Bind _ tyExpr )        = ftv tyExpr
      ftv' (SubtypeBind _ tyExpr ) = ftv tyExpr
      ftv' (RefineBind _ tyExpr )  = ftv tyExpr
  ftv (TImplementation prtcl tyExpr)  = ftv tyExpr
  ftv (TQualified (cs :=> ty))        = cs' `Set.union` ty'
    where
      cs' = ftv cs
      ty' = ftv ty
  ftv (TIdentifier id)          = Set.singleton $ Tyvar id KType
instance Substitutable CompositeExpr where
  apply s (TEUnion tys)      = TEUnion $ apply s tys
  apply s (TETuple tys)      = TETuple $ apply s tys
  apply s (TERecord pairs)   = TERecord $ fmap (fmap (apply s)) pairs
  apply s (TEArrow in' out') =  apply s in' `TEArrow` apply s out'

  ftv (TETuple elems)  = ftv elems
  ftv (TERecord pairs) = ftv pairs
  ftv (TEUnion tys)    = ftv tys
  ftv (t `TEArrow` t') = ftv t `Set.union` ftv t'

instance Substitutable (Binding TypeExpr) where
  apply s (ImplBind tyExpr prtcl) = ImplBind (apply s tyExpr) prtcl
  apply _ b                       = b

  ftv (ImplBind tyExpr _) = ftv tyExpr
  ftv _                   = Set.empty


instance Substitutable InferenceEnv where
  --apply s t | trace ("Applying type env sub\n\t" ++ show s) False = undefined
  apply s e@(Env vars aliases) = e {unificationVars = Map.map (apply s) vars}
  ftv (Env vars aliases) = ftv $ Map.elems vars

instance Substitutable IConstraint where
   apply s (EqCons eq)   = EqCons $ apply s eq
   apply s (ImplCons ip) = ImplCons $ apply s ip
   apply s c             = c
   ftv (EqCons eq)   = ftv eq
   ftv (ImplCons ip) = ftv ip

instance Substitutable ImplConstraint where
  --apply s ip | trace ("Applying IP constraint sub\n\t" ++ show s ++ "\n\t" ++ show ip) False = undefined
  apply s  (t `IP` p) = apply s t `IP` p

  ftv (t `IP` p) = ftv  t
instance Substitutable Equality where
  --apply s e | trace ("Applying EQ constraint sub\n\t" ++ show s ++ "\n\t" ++ show e) False = undefined
  apply s  (t1 `EQ` t2) = apply s t1 `EQ` apply s t2
  ftv (t1 `EQ` t2) = ftv t1 `Set.union` ftv t2
instance Substitutable Union where
  --apply s e | trace ("Applying EQ constraint sub\n\t" ++ show s ++ "\n\t" ++ show e) False = undefined
  apply s  (t1 `MemberOf` t2) = apply s t1 `MemberOf` apply s t2
  ftv (t1 `MemberOf` t2) = ftv t1 `Set.union` ftv t2

instance Substitutable Constraint where
  apply s (t `Implements` p) = apply s t `Implements` p

  ftv (t `Implements` p) = ftv t



instance Substitutable Expr where
  apply s (Typed e t)        = Typed (apply s e) (apply s t)
  apply s (Lambda ps body)   = Lambda ps (apply s body)
  apply s (FnApp fn args)    = FnApp (apply s fn) (apply s args)
  apply s (Match cond cases) = Match (apply s cond) (apply s cases)
  apply s (Tuple es)         = Tuple (apply s es)
  apply s (List es)          = List (apply s es)
  apply s (Record pairs)     = Record $ apply s <$> pairs
  apply s (Block stmts)      = Block $ apply s stmts
  apply s e                  = e

  ftv (Typed e t)        = ftv e <> ftv t
  ftv (Lambda _ body)    = ftv body
  ftv (FnApp fn args)    = ftv fn <> ftv args
  ftv (Match cond cases) = ftv cond <> ftv cases
  ftv (Tuple es)         = ftv es
  ftv (List es)          = ftv es
  ftv (Record pairs)     = ftv pairs
  ftv (Block stmts)      = ftv stmts
  ftv _                  = mempty

instance Substitutable Case where
  apply s (Case pat e)         = Case pat (apply s e)
  apply s (TypedCase pat ty e) = TypedCase pat (apply s ty) (apply s e)

  ftv (Case _ e)         = ftv e
  ftv (TypedCase _ ty e) = ftv e <> ftv ty

instance Substitutable Statement where
  apply s (Return e)      = Return $ apply s e
  apply s (Procedure e)   = Procedure $ apply s e
  apply s (Declaration d) = Declaration $ apply s d

  ftv (Return e)      = ftv e
  ftv (Procedure e)   = ftv e
  ftv (Declaration d) = ftv d

instance Substitutable Declaration where
  apply s (Let id ty k e)    = Let id (apply s ty) k (apply s e)
  apply s (Type id k ty)     = Type id k (apply s ty)
  apply s (Data id k tyExpr) = Data id k (apply s tyExpr)

  ftv (Let _ ty _ e)    = ftv ty <> ftv e
  ftv (Type _ _ ty)     = ftv ty
  ftv (Data _ _ tyExpr) = ftv tyExpr


compose :: Subst -> Subst -> Subst
s1 `compose` s2 = s `Map.union` s1
  where
    s = Map.map (apply s1) s2





class HasKind t where
  kind :: t -> Kind

instance HasKind Tyvar where
  kind (Tyvar _ k) = k

instance HasKind Tycon where
  kind (Tycon _ k) = k
instance HasKind TypeExpr where
  kind (TAtom ty) = kind ty
  kind (TComposite composite) = kind composite
  kind (TTagged tag ty) = kind ty
  kind (TClause ty binds) = kind ty
  kind (TImplementation pid ty) = kind ty
  kind (TLambda params body) = foldr (KArrow . KVar) (kind body) params
  kind (TIdentifier ty) = error "still need to implement kind inference for TIdentifier"
  kind (TFnApp fn args) = error "still need to implement kind inference for TFnApp"
  kind (TConditional {}) = error "still need to implement kind inference for TConditional"
  kind (TQualified (cs :=> ty )) = kind ty

instance HasKind CompositeExpr where
  kind (TEUnion tys) =
      if all (== k) (fmap kind tys) then
        k
      else error "Mismatching kinds in type union"
      where k =  kind $ head tys
  kind (TERecord {}) = KType -- TODO: Is this correct? what happens when a key's value is a type lambda?
  kind (TETuple {}) = KType -- TODO: Same here. KType seems correct, but how and when to enforce it?
  kind (TEArrow {}) = KType -- TODO: This probably can be investigated by looking into rank-N types
instance HasKind Type where
  kind (TData cons) = kind cons
  kind (TVar u)     = kind u
  kind (TApplied f _) = case kind f of
    (KArrow _ k) -> k
  kind (TClosure t tyExpr _) = kind tyExpr
  kind _ = KType
  --   (KArrow _ k) -> k

