

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

import           Debug.Trace                                   (trace, traceM)
import           Saga.AST.TypeSystem.HindleyMilner.Types
import           Saga.Parser.ParserHM                          (runSagaExpr)
import           Saga.Parser.ParsingInfo

-- import           Control.Monad.State.Lazy
-- import           Control.Monad.Trans.Except    (ExceptT)
-- import qualified Data.Map                      as Map
-- import qualified Data.Set                      as Set
-- import           Saga.AST.Syntax               (Expr, Name (..))
-- import           Saga.AST.TypeSystem.Inference hiding (Infer, TypeError, fresh,
--                                                 letters)
-- import           Saga.AST.TypeSystem.Types



run :: String -> Either String Scheme
run input = do
    Parsed expr _ _ <- runSagaExpr input
    show `first` runInfer (infer expr)




runInfer :: Infer (Subst, Type) -> Either TypeError Scheme
runInfer m = runExcept $ evalStateT inference [empty]
  where
    inference = do
      res <- m
      scheme <- closeOver res
      normalize scheme


closeOver :: (Map.Map TVar Type, Type) -> Infer Scheme
closeOver (sub, ty) = do
  sub' <- apply sub ty
  scheme <- generalize sub'
  normalize scheme



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
  apply s t | trace ("Applying type sub: " ++ show s ++ " to " ++ show t) False = undefined

  apply s t@(TVar id)     = return $ Map.findWithDefault t id s
  apply s (inExpr `TArrow` outExpr) = do
    inTy <- refine inExpr
    outTy <- refine outExpr
    in'  <- apply s inTy
    out' <- apply s outTy
    traceM $ "Result: " ++ show (Type in' `TArrow` Type out')
    return $ Type in' `TArrow` Type out'

  apply _ ty       = return ty

  ftv (TVar id)       = return $ Set.singleton id
  ftv (t `TArrow` t') = do
    set <- refine t >>= ftv
    set' <- refine t' >>= ftv
    return $ set `Set.union` set'
  ftv _         = return Set.empty

instance Substitutable Infer Scheme where
  apply s t | trace ("Applying scheme sub: " ++ show s ++ " to " ++ show t) False = undefined
  apply s (Scheme as t) =
    let
      s' = foldr Map.delete s as
    in do
      ty <- apply s' t
      traceM $ "Result: " ++ show (Scheme as ty)
      return $ Scheme as ty


  ftv (Scheme as t) = do
    set <- ftv t
    return $ set `Set.difference` Set.fromList as



instance Substitutable Infer TypeEnv where
  apply s t | trace ("Applying type env sub: " ++ show s ++ " to " ++ show (typeVars t)) False = undefined
  apply s e@(Env vars aliases count) = do
    typeVars' <- sequence $ Map.map (apply s) vars

    traceM $ "Result: " ++ show typeVars'
    return $ e{ typeVars = typeVars' }
  ftv (Env vars aliases count) = ftv $ Map.elems vars




compose :: Subst -> Subst  -> Infer Subst
s `compose` s' = do
  s'' <- sequence $ Map.map (apply s) s'
  return $ s'' `Map.union` s

unify ::  Type -> Type -> Infer Subst
unify t1 t2 | trace ("Unifying: " ++ show t1 ++ " with " ++ show t2) False = undefined

unify (inL `TArrow` outL) (inR `TArrow` outR) = do
  il <- refine inL
  ir <- refine inR
  ol <- refine outL
  or <- refine outR

  s <- unify il ir
  t  <- apply s ol
  t'  <- apply s or
  s' <- unify t t'

  s' `compose` s

unify (TVar a) t = bind a t
unify t (TVar a) = bind a t
-- unify (TParametric args cons) t = do
--   (TParametric params body) <- refine cons
--   args' <- mapM refine args
--   tVars <- mapM ftv params
--   return $ nullSubst

unify (TPrimitive a) (TPrimitive b) | a == b = return nullSubst
unify (TLiteral a) (TLiteral b) | a == b = return nullSubst
unify (TTuple as) (TTuple bs) = do
  as' <- mapM refine as
  bs' <- mapM refine bs
  ss <- zipWithM unify as' bs'
  foldM compose nullSubst ss

unify sub@(TRecord as) parent@(TRecord bs) = sub `isSubtype` parent
  -- as' <- mapM refine' as
  -- bs' <- mapM refine' bs
  -- return nullSubst
  -- where
  --   refine' (str, tyExpr) = do
  --     ty <- refine tyExpr
  --     return (str, ty)


unify t t' = throwError $ UnificationFail t t'

bind ::  TVar -> Type -> Infer Subst
bind a t | trace ("Binding: " ++ show a ++ " to " ++ show t) False = undefined
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


generalize :: Type -> Infer Scheme
generalize t | trace ("Generalizing: " ++ show t) False = undefined
generalize t = do
    t' <- ftv t
    env <- get
    tVars <- ftv env
    let as = Set.toList $ t' `Set.difference` tVars
    return $ Scheme as t



-- -- ops :: Binop -> Type
-- -- ops Add = typeInt `TArr` typeInt `TArr` typeInt
-- -- ops Mul = typeInt `TArr` typeInt `TArr` typeInt
-- -- ops Sub = typeInt `TArr` typeInt `TArr` typeInt
-- -- ops Eql = typeInt `TArr` typeInt `TArr` typeBool

lookupEnv :: TVar -> Infer (Subst, Type)
lookupEnv x = do
  (Env vars aliases count) <- get
  case Map.lookup x aliases of
    Just tyExpr -> do
      ty <- refine tyExpr
      return (nullSubst, ty)
    Nothing -> case Map.lookup x vars of
      Nothing -> throwError $ UnboundVariable (show x)
      Just s  -> do t <- instantiate s
                    return (nullSubst, t)

infer :: Expr -> Infer (Subst, Type)
infer ex | trace ("Inferring: " ++ show ex) False = undefined
infer ex = case ex of

  Identifier x -> lookupEnv x


  Lambda (param:rest) body -> do
    tvar <- fresh
    modify $ \env -> env `extend` (param, Scheme [] tvar)
    (s, t) <- infer $ case rest of
      [] -> body
      _  -> Lambda rest body
    t' <- apply s tvar
    return (s, Type t' `TArrow` Type t) -- TO


  FnApp fn args -> do
    tv <- fresh
    (s, t) <- infer fn
    modifyM $ apply s
    (s', t') <- infer $ case args of
      [body] -> body
      a:as   -> FnApp a as

    ty <- apply s' t
    s3       <- unify ty (Type t' `TArrow` Type tv)
    sub <- s3 `compose` s'
    sub' <- sub `compose` s
    ty' <- apply s3 tv
    return (sub, ty')


  Assign x e -> do
    (s, t) <- infer e
    modifyM $ apply s
    t'   <- generalize t
    modify $ \env -> env `extend` (x, t')
    return (s, t)

  IfElse cond tr fl -> do
    tv <- fresh
    let tArrow t t' = Type t `TArrow` Type t'
    inferPrim [cond, tr, fl] $ TPrimitive TBool `tArrow` tv `tArrow` tv `tArrow` tv

  -- Fix e -> do
  --   tv <- fresh
  --   inferPrim env [e] ((tv `TArr` tv) `TArr` tv)

  -- Op op e e' -> do
  --   inferPrim env [e, e'] (ops op)

  Term (LTuple elems) -> do
    tElems <- mapM infer' elems
    return (nullSubst, TTuple tElems)
    where
      infer' = extract . infer
      extract = fmap (Type . snd)

  Term (LRecord pairs) -> do
    tPairs <- mapM infer' pairs
    return (nullSubst, TRecord tPairs)
    where
      infer' = mapM $ extract . infer
      extract = fmap (Type . snd)

  Term (LInt i)  -> return (nullSubst, TLiteral (LInt i))
  Term (LBool b) -> return (nullSubst, TLiteral (LBool b))
  Term (LString s) -> return (nullSubst, TLiteral (LString s))


inferPrim :: [Expr] -> Type -> Infer (Subst, Type)
inferPrim l t = do
  tv <- fresh
  (s, tf) <- foldM inferStep (nullSubst, id) l
  t' <- apply s (tf tv)
  s' <- unify t' t
  sub <- s' `compose` s
  t'' <-  apply s' tv
  return (sub, t'')
    where
      inferStep (s, tf) exp = do
        modifyM $ apply s
        (s', t) <- infer exp
        sub <- s' `compose` s
        return (sub, tf . TArrow (Type t) . Type)

inferExpr ::  Expr -> Either TypeError Scheme
inferExpr = runInfer . infer

-- inferTop ::  [(String, Expr)] -> Either TypeError TypeEnv
-- inferTop [] = Right env
-- inferTop ((name, ex):xs) = case inferExpr env ex of
--   Left err -> Left err
--   Right ty -> inferTop (extend env (name, ty)) xs

normalize :: Scheme -> Infer Scheme
normalize sc | trace ("Normalizing: " ++ show sc) False = undefined
normalize (Scheme ts body) = do
  body' <- fv body
  let ord = zip (nub body') letters
  ty <- normtype body ord
  return $ Scheme (fmap snd ord) ty

  where


    fv (TVar a)   = return [a]
    fv (TArrow a b) = do
      a' <- refine a
      b' <- refine b
      fva <- fv a'
      fvb <- fv b'
      return $ fva ++ fvb
    fv _   = return []

    normtype (TArrow a b) ord = do
      a' <- refine a
      b' <- refine b
      ta <- normtype a' ord
      tb <- normtype b' ord
      return $ TArrow (Type ta) (Type tb)

    normtype (TVar a) ord = return $
      case lookup a ord of
        Just x  -> TVar x
        Nothing -> error "type variable not in signature"
    normtype ty _  = return $ ty



refine :: TypeExpr -> Infer Type
refine a | trace ("refining: " ++ show a) False = undefined
refine (Type ty)                      = return ty
refine (TParens tyExp)                = refine tyExp
refine (TClause _ tyExp)              = refine tyExp
refine (TBlock [])                    = return TUnit
refine (TBlock tyExps)                = refine $ last tyExps
refine (TReturn tyExp)                = refine tyExp
refine (TConditional cond true false) = refine true -- assumes same return type, will change with union types
refine (TIdentifier id)               = snd <$> lookupEnv id

refine (TFnApp fnExpr argExprs)    = do
    args <- mapM refine argExprs
    constructor <- refine fnExpr

    traceM $ "Fn Expression: " <> show constructor
    traceM $ "Fn Args: " <> show args


    case (constructor, argExprs) of
        (TVar t, []) -> snd <$> lookupEnv t
        -- (TVar t, [last]) -> return $ TParametric t last
        -- (TVar id, tyExpr:tail) -> do
        --     out <- evalScoped $ refine $ TFnApp info tyExpr tail
        --     return $ TParametric t (Type out)

        (TParametric _ body, []) -> refine body
        (TParametric [param] body, [tyExpr]) -> do
            ty <- refine tyExpr
            bind param ty
            refine body
        (TParametric (p:ps) body, tyExpr:es) -> do
            ty <- refine tyExpr
            bind p ty
            let lambda = TLambda ps body
            refine $ TFnApp lambda es

        _ -> throwError $ UnexpectedType "Cannot apply this type expression"



refine (TLambda params body) = return $ TParametric params body





isSubtype :: Type -> Type -> Infer Subst
-- isSubtype  a b | trace ("subtype " ++ show a ++ " <: " ++ show b ++ "\n  ") False = undefined
TLiteral (LInt _) `isSubtype` TPrimitive TInt = return nullSubst
TLiteral (LString _) `isSubtype` TPrimitive TString = return nullSubst
TLiteral (LBool _) `isSubtype` TPrimitive TBool = return nullSubst
TPrimitive prim1 `isSubtype` TPrimitive prim2  | prim1 == prim2 = return nullSubst
sub@(TRecord pairs1) `isSubtype` parent@(TRecord pairs2) = do
  pairs1' <- mapM (mapM refine) pairs1
  pairs2' <- mapM (mapM refine) pairs2
  let check (name, ty2) = case lookup name pairs1' of
        Nothing  -> throwError $ UnificationFail sub parent
        Just ty1 -> unify ty1 ty2

  subs <- mapM check pairs2'
  foldM compose nullSubst subs

sub `isSubtype` parent = throwError $ SubtypeFailure sub parent
