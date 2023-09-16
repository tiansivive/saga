{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
module Saga.Language.TypeSystem.HindleyMilner.Constraints where

import           Control.Monad.Except
import           Control.Monad.Reader                               (MonadReader (local),
                                                                     ReaderT (runReaderT))
import           Control.Monad.RWS                                  (MonadReader (ask),
                                                                     RWST)
import           Control.Monad.State                                (StateT (runStateT),
                                                                     evalStateT,
                                                                     get,
                                                                     modify,
                                                                     put)
import           Data.Bifunctor                                     (bimap)
import           Data.Either                                        (fromRight)
import           Data.Functor                                       ((<&>))
import           Data.List                                          (delete,
                                                                     groupBy,
                                                                     intersect,
                                                                     partition,
                                                                     (\\))
import qualified Data.Map                                           as Map
import           Data.Maybe                                         (fromJust,
                                                                     fromMaybe)
import qualified Data.Set                                           as Set
import           Debug.Trace
import           Prelude                                            hiding (EQ)
import           Saga.Language.Core.Literals                        (Literal (..))
import           Saga.Language.TypeSystem.HindleyMilner.Environment hiding
                                                                    (Implements)
import           Saga.Language.TypeSystem.HindleyMilner.Lib
import qualified Saga.Language.TypeSystem.HindleyMilner.Refinement  as Refine
import           Saga.Language.TypeSystem.HindleyMilner.Types       hiding
                                                                    (ProtocolID,
                                                                     implementationTy)
import           Text.Pretty.Simple                                 (pShow)
-- type Solve = StateT SolveState (Except InferenceError)

type Solve = ReaderT ProtocolEnv (Except InferenceError)
type ProtocolEnv = Map.Map ProtocolID Protocol

type Subst = Map.Map UnificationVar Type

data SolveState = SST { unifier :: (Subst, [IConstraint]), protocols :: Map.Map ProtocolID Protocol}


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
  apply s t@(TVar id) = Map.findWithDefault t id s
  apply s (inTy `TArrow` outTy) = in' `TArrow` out'
    where
      in' = apply s inTy
      out' = apply s outTy

  apply _ ty = ty

  ftv (TVar id)                = Set.singleton id
  ftv (TTuple elems)           = ftv elems
  ftv (TRecord pairs)          = ftv pairs
  ftv (TUnion tys)             = ftv tys
  ftv (cons `TApplied` arg)    = ftv cons `Set.union` ftv arg
  ftv (t `TArrow` t')          = ftv t `Set.union` ftv t'
  ftv (TClosure params body _) = ftv body
  ftv _                        = Set.empty

instance Substitutable TypeExpr where
  apply s t | trace ("Applying typeExpr sub: " ++ show s ++ " to " ++ show t) False = undefined
  apply s (TAtom ty) = TAtom $ apply s ty
  apply s (TLambda params tyExpr) = TLambda (subStrVar s <$> params) $ apply s tyExpr
    where
      subStrVar sub str = case Map.lookup (Tyvar str KType) sub of
        Nothing                   -> str
        Just (TVar (Tyvar id _ )) -> id
  apply s (TQualified (cs :=> ty)) = TQualified (cs' :=> ty')
    where
      ty' = apply s ty
      cs' = apply s <$> cs
  apply _ t  = t

  ftv (TAtom ty) = ftv ty
  ftv (TLambda _ tyExpr) = ftv tyExpr
  ftv (TQualified (cs :=> ty)) =  cs' `Set.union` ty'
    where
      cs' = ftv cs
      ty' = ftv ty

instance Substitutable Scheme where
  --apply s t | trace ("Applying scheme sub: " ++ show s ++ " to " ++ show t) False = undefined
  apply s (Scheme k (cs :=> ty)) = Scheme k (cs' :=> ty')
    where
      ty' = apply s ty
      cs' = apply s <$> cs

  ftv (Scheme k (cs :=> ty)) = cs' `Set.difference` ty'
    where
      cs' = ftv cs
      ty' = ftv ty

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

instance Substitutable Constraint where
  apply s (t `Implements` p) = apply s t `Implements` p

  ftv (t `Implements` p) = ftv t

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = s `Map.union` s1
  where
    s = Map.map (apply s1) s2



runSolve :: [IConstraint] -> Either InferenceError (Subst, [ImplConstraint])
-- runSolve cs | trace ("Solving: " ++ show cs) False = undefined
runSolve cs =  runExcept $ runReaderT (solver cs) builtInProtocols



solver :: [IConstraint] -> Solve (Subst, [ImplConstraint])
solver constraints = do
  let eqs' = eqs constraints
  --traceM $ "EQs: " ++ show eqs'
  sub <- unification nullSubst eqs'
  --traceM $ "\nMGU:\n\t" ++ show sub
  is <- reduce $ apply sub $ impls constraints
  resolve is
  return (sub, is)


  where
    impls cs =  [ ip | ImplCons ip <- cs ]
    eqs   cs =  [ eq | EqCons   eq <- cs ]



unification :: Subst -> [Equality] -> Solve Subst
--unification s cs | trace ("\nUnification:" ++ "\n\tUnifier: " ++ show s ++ "\n\tConstraints: " ++ show cs) False = undefined
unification s [] = return s
unification s (e:es) | t1 `EQ` t2 <- e = do
  sub <- unify t1 t2
  let sub' = compose sub s
  sub'' <- unification sub' $ apply sub' es
  return $ sub'' `compose` sub'




unify :: (MonadError e m, e ~ InferenceError) => Type -> Type -> m Subst
--unify t1 t2 | trace ("Unifying:\n\t" ++ show t1 ++ "\n\t" ++ show t2) False = undefined

unify (TLiteral a) (TLiteral b) | a == b = return nullSubst
unify (TPrimitive a) (TPrimitive b) | a == b = return nullSubst
unify (TData lCons) (TData rCons) | lCons == rCons = return nullSubst
unify sub@(TRecord as) parent@(TRecord bs) = sub `isSubtype` parent
unify lit@(TLiteral _) prim@(TPrimitive _) = lit `isSubtype` prim
unify (TTuple as) (TTuple bs) = do
  ss <- zipWithM unify as bs
  return $ foldl compose nullSubst ss

unify (il `TArrow` ol) (ir `TArrow` or) = do
  sub <- unify il ir
  s <- apply sub ol `unify` apply sub or
  return $ s `compose` sub
unify (TApplied f t) (TApplied f' t') = do
  sub <- unify f f'
  s <- apply sub t `unify` apply sub t'
  return $ s `compose` sub

unify (TVar a) t = bind a t
unify t (TVar a) = bind a t

unify t t' | kind t /= kind t' = throwError $ Fail "Kind mismatch"
unify t1 t2 = case (t1, t2) of
  (t@(TClosure {}), t') -> refine t `unify` t'
  (t, t'@(TClosure {})) -> t `unify` refine t'
  _                     -> throwError $ UnificationFail t1 t2
  where
    refine (TClosure params tyExpr env) = fromRight err $ Refine.runIn (env `Map.union` env') tyExpr
      where
        tvars = fmap (`Tyvar` KType) params
        env'  = Map.fromList $ zip params (fmap TVar tvars)
        err   = error "Failed to refine TClosure while unifying"


bind :: (MonadError e m, e ~ InferenceError) => Tyvar -> Type -> m Subst
-- bind a t | trace ("Binding: " ++ show a ++ " to " ++ show t) False = undefined
bind a t
  | t == TVar a = return nullSubst
  | occursCheck a t = throwError $ InfiniteType a t
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

isSubtype :: (MonadError e m, e ~ InferenceError) => Type -> Type -> m Subst
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
-- resolve cs | trace ("\n\nProtocol Resolution:\n\t" ++ show cs) False = undefined
resolve constraints = do
  env <- ask
  --traceM $ "\tGrouped Constraints: " ++ show (groupBy byType constraints)
  --traceM $ "\tGrouped Implementations: " ++ show (groupBy byType' (impls env))
  forM_ constraintGroups findImplementingType

  where
    findImplementingType :: [ProtocolID] -> Solve ()
    findImplementingType ps = do
      env <- ask
      if any' (implGroups env) (hasAll ps)
        then return ()
        else throwError $ Fail $ "Could not resolve for protocols: " ++ show ps

    impls = concatMap implementations . Map.elems
    byType (ty `IP` _) (ty' `IP` _) = ty == ty'
    extract (IP _ p) = p

    byType' (_ :=> IP ty _) (_ :=> IP ty' _) = ty == ty'
    extract' (_ :=> IP _ p) = p

    constraintGroups = fmap extract <$> groupBy byType constraints
    implGroups env = fmap extract' <$> groupBy byType' (impls env)

    hasAll as bs = as `intersect`  bs == as
    any' = flip any


reduce :: [ImplConstraint] -> Solve [ImplConstraint]
-- reduce cs | trace ("\nReducing\n\tImplementation constraint:" ++ show cs) False = undefined
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
-- byImplementation impl | trace ("\nSearch by Implementation:\n\t" ++ show impl) False = undefined
byImplementation implConstraint@(ty `IP` p)    = do
  env <- ask
  concat <$> sequence [ tryInst impl | impl <- impls env p ]

  where
    mkIP (ty `Implements` p) = ty `IP` p
    impls env id = maybe [] implementations $ Map.lookup id env
    tryInst (cs :=> implConstraint') = do
      sub <- unifyImpl implConstraint' implConstraint
      return $ fmap (apply sub . mkIP) cs

unifyImpl :: ImplConstraint -> ImplConstraint -> Solve Subst
-- unifyImpl p1 p2 | trace ("\n-------\nUnifying Implementations\n-------\n\t" ++ show p1 ++ "\n\t" ++ show p2 ++ "\n") False = undefined
unifyImpl (ty `IP` p) (ty' `IP` p')
  | p == p'   = ty `match` ty'
  | otherwise = throwError $ Fail "protocols differ"

  where
    match :: Type -> Type -> Solve Subst
    match (TVar v)   ty     = return $ Map.fromList [(v, ty)]
    match ty    (TVar v)    = return $ Map.fromList [(v, ty)]
    match t1 t2 | t1 == t2  = return nullSubst
    match t1 t2             = throwError $ Fail "types do not match"


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
    protocols <- ask
    impls <- sequence [ byBase (ty `IP` base) | base <- sups protocols p ]
    let all = impl : concat impls
    --traceM $ "Found:\n\t" ++ show all
    return all
    where
      sups env id = maybe [] supers $ Map.lookup id env







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
  kind (TImplementation pid ty) = KProtocol
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

