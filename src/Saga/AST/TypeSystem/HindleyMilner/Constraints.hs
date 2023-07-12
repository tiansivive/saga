{-# LANGUAGE LambdaCase #-}


module Saga.AST.TypeSystem.HindleyMilner.Constraints where

import           Control.Monad.Except
import           Control.Monad.State                           (StateT (runStateT),
                                                                evalStateT, get,
                                                                modify, put)
import           Saga.AST.TypeSystem.HindleyMilner.Environment

import           Data.Bifunctor                                (bimap)
import           Data.List                                     (delete,
                                                                partition, (\\))
import qualified Data.Map                                      as Map
import qualified Data.Set                                      as Set
import           Debug.Trace
import           Saga.AST.TypeSystem.HindleyMilner.Types       hiding
                                                               (ProtocolId)
import           Text.Pretty.Simple                            (pShow)


type Solve = StateT SolveState (Except InferenceError)

type Subst = Map.Map UnificationVar Type

data SolveState = SST { unifier :: (Subst, [IConstraint]), protocols :: Map.Map ProtocolId Protocol }


class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set.Set UnificationVar

instance (Substitutable a) => Substitutable [a] where
  apply = map . apply

  ftv = foldl union Set.empty
    where
      union set x = Set.union (ftv x) set

instance Substitutable Type where
  apply s t | trace ("Applying type sub\n\t" ++ show s ++ "\n\t" ++ show t) False = undefined
  apply s t@(TVar id) = Map.findWithDefault t id s
  apply s (inTy `TArrow` outTy) = in' `TArrow` out'
    where
      in' = apply s inTy
      out' = apply s outTy
--   apply s (TConstrained cs ty) = apply s ty
  apply _ ty = ty

  ftv (TVar id) = Set.singleton id
  ftv (t `TArrow` t') = set `Set.union` set'
    where
      set = ftv t
      set' = ftv t'
  ftv _ = Set.empty

instance Substitutable Scheme where
  apply s t | trace ("Applying scheme sub: " ++ show s ++ " to " ++ show t) False = undefined
  apply s (Scheme as ( _ :=> t)) = Scheme as ([] :=> ty)
    where
      s' = foldr Map.delete s as
      ty = apply s' t

  ftv (Scheme as ( _ :=> t)) = set `Set.difference` Set.fromList as
    where
      set = ftv t

instance Substitutable TypeEnv where
  apply s t | trace ("Applying type env sub\n\t" ++ show s ) False = undefined
  apply s e@(Env vars aliases) = e {unificationVars = Map.map (apply s) vars}
  ftv (Env vars aliases ) = ftv $ Map.elems vars

instance Substitutable IConstraint where
   apply s (t1 `Equals` t2) = apply s t1 `Equals` apply s t2
   apply s c                = c
   ftv (t1 `Equals` t2) = ftv t1 `Set.union` ftv t2

instance Substitutable Constraint where
   apply s (t `Implements` p) = apply s t `Implements` p

   ftv (t `Implements` p) = ftv t


runSolve :: [IConstraint] -> Either InferenceError Subst
runSolve cs | trace ("Solving: " ++ show cs ) False = undefined
runSolve cs = runExcept $ evalStateT solver st
  where st = SST (nullSubst, cs) builtInProtocols

solver :: Solve Subst
solver = do
  SST (sub, constraints) _ <- get
  traceM $ "-------------------\nSolve state" ++ "\n\tsubs: " ++ show sub ++ "\n\tConstraints: " ++ show constraints ++ "\n"
  case constraints of
    [] -> return sub
    (c: cs) -> case c of
        Equals t1 t2 -> do
            sub'  <- unify t1 t2
            modify $ \st -> st{ unifier = bimap (compose sub') (apply sub' . delete c) $ unifier st }
            solver
        _ -> do -- throwError $ Fail "Not implemented yet"
            modify $ \st -> st{  unifier = delete c <$> unifier st }
            solver

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = s `Map.union` s1
  where
    s = Map.map (apply s1) s2

unify :: Type -> Type -> Solve Subst
unify t1 t2 | trace ("Unifying:\n\t" ++ show t1 ++ "\n\t" ++ show t2) False = undefined
unify (il `TArrow` ol) (ir `TArrow` or) = do
  sub <- unify il ir
  s <- apply sub ol `unify` apply sub or
  return $ s `compose` sub
unify (TVar a) t = bind a t
unify t (TVar a) = bind a t
unify (TPrimitive a) (TPrimitive b) | a == b = return nullSubst
unify (TLiteral a) (TLiteral b) | a == b = return nullSubst
unify (TTuple as) (TTuple bs) = do
  ss <- zipWithM unify as bs
  return $ foldl compose nullSubst ss
unify sub@(TRecord as) parent@(TRecord bs) = sub `isSubtype` parent
-- unify (TConstrained constraints ty) ty' = do
--     modify $ \(sub, cs) -> (sub, cs ++ fmap protocol constraints)
--     unify ty ty'
--     where
--         protocol (t `Implements` protocol) = Protocol protocol [t]
-- unify ty (TConstrained constraints ty') = do
--     modify $ \(sub, cs) -> (sub, cs ++ fmap protocol constraints)
--     unify ty ty'
--     where
--         protocol (t `Implements` protocol) = Protocol protocol [t]
unify t t' = throwError $ UnificationFail t t'


bind :: UnificationVar -> Type -> Solve Subst
bind a t | trace ("Binding: " ++ a ++ " to " ++ show t) False = undefined
bind a t
  | t == TVar a = return nullSubst
  | occursCheck a t = throwError $ InfiniteType a t
  | TLiteral l <- t = return . Map.singleton a $
    case l of
        LInt _    ->  TPrimitive TInt
        LString _ -> TPrimitive TString
        LBool _   -> TPrimitive TBool
  | otherwise = return $ Map.singleton a t

occursCheck :: Substitutable a => UnificationVar -> a -> Bool
occursCheck a t = a `Set.member` set
  where
    set = ftv t


nullSubst :: Subst
nullSubst = Map.empty



isSubtype :: Type -> Type -> Solve Subst
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




entail :: [Constraint] -> Constraint -> Solve Bool
entail cs c = do
  SST { protocols } <- get
  cs' <- mapM bySuper cs

  if any (c `elem`) cs'
    then return True
    else do
      maybeImpls <- byImplementation c
      case maybeImpls of
        Nothing -> return False
        Just qs -> and <$> mapM (entail cs) qs

bySuper :: Constraint -> Solve [Constraint]
bySuper c@(ty `Implements` p) = do
    env <- get
    cs <- sequence [ bySuper (ty `Implements` p') | p' <- sups env p ]
    return $ c : concat cs
    where
      sups env id = maybe [] supers $ Map.lookup id $ protocols env



byImplementation :: Constraint -> Solve (Maybe [Constraint])
byImplementation c@(ty `Implements` p)    = do
  env <- get
  return $ msum [ tryInst p' | p' <- impls env p ]
  where
    impls env id = maybe [] implementations $ Map.lookup id $ protocols env
    tryInst (cs :=> ty') = do
      u <- matchConstraint ty' c
      Just (map (apply u) cs)


matchConstraint :: Constraint -> Constraint -> Maybe Subst
matchConstraint  = liftC match

liftC :: MonadFail m => (Type -> Type -> m a) -> Constraint -> Constraint -> m a
liftC m (ty `Implements` p) (ty' `Implements` p')
         | p == p'   = m ty ty'
         | otherwise = fail "classes differ"

match :: MonadFail m => Type -> Type -> m Subst
match (l `TArrow` r) ( l' `TArrow` r')              = do
    sl <- match l l'
    sr <- match r r'
    merge sl sr
match (TVar u)   t     = return $ Map.fromList [(u, t)]
match t1 t2 | t1 == t2  = return nullSubst
match t1 t2             = fail "types do not match"

merge      :: MonadFail m => Subst -> Subst -> m Subst
merge s1 s2 = if agree then return (s1 `compose` s2) else fail "merge fails"
  where
    agree = all (\v -> apply s1 (TVar v) == apply s2 (TVar v)) uni
    uni = Map.keys $ Map.intersection s1 s2



inHNF :: Constraint -> Bool
inHNF (ty `Implements` p) = hnf ty
 where hnf (TVar v) = True
       hnf _        = False

toHNF :: Constraint -> Solve [Constraint]
toHNF c | inHNF c   = return [c]
        | otherwise = byImplementation c >>= \case
            Nothing -> throwError $ Fail "context reduction"
            Just cs -> do
              cs' <- mapM toHNF cs
              return $ concat cs'

reduce :: [Constraint] -> Solve [Constraint]
reduce cs = mapM toHNF cs >>= simplify . concat


simplify   :: [Constraint] -> Solve [Constraint]
simplify = loop []
 where
  loop checked []     = return checked
  loop checked (c:cs) = do
    yes <- entail (checked ++ cs) c
    if yes
      then loop checked cs
      else loop (c: checked) cs


type Ambiguity = (TyVar, [Constraint])

ambiguities :: [TyVar] -> [Constraint] -> [Ambiguity]
ambiguities vars cs = do
  v <- Set.toList (ftv cs) \\ vars
  return (v, filter (elem v . ftv) cs)


candidates :: Ambiguity -> Solve [Type]
candidates (var, cs) = do
  cs' <- filterM (entail []) cs
  return $ implementationTy <$> cs'
  where
    ps =  [ p  | ty `Implements` p <- cs ]
    tys = [ ty | ty `Implements` p <- cs ]
    cs =  [ t' `Implements` p | all (TVar var ==) tys,
                                any (`elem` numProtocols) ps,
                                all (`elem` stdProtocols) ps,
                                p <- ps,
                                t' <- builtInTypes
          ]

-- | Find the defaults for a combo of type vars and constraints
withDefaults :: ([Ambiguity] -> [Type] -> a) -> [TyVar] -> [Constraint] -> Solve a
withDefaults f vars cs = do
    tys' <- mapM candidates as
    if any null tys'
      then throwError $ Fail "cannot resolve ambiguity"
      else return $ f as $ map head tys'

    where
      as = ambiguities vars cs


defaultedConstraints:: [TyVar] -> [Constraint] -> Solve [Constraint]
defaultedConstraints = withDefaults (\as _ -> concatMap snd as)

split :: [TyVar] -> [TyVar] -> [Constraint] -> Solve ([Constraint], [Constraint])
split vars vars' cs = do
  cs' <- reduce cs
  let (deferred, retained) = partition (all (`elem` vars) . ftv) cs'
  retained' <- defaultedConstraints (vars ++ vars') retained
  return (deferred, retained \\ retained')

