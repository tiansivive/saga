{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}


module Saga.AST.TypeSystem.Inference where

import           Control.Monad.Except
import           Control.Monad.Identity       (Identity (runIdentity))
import           Control.Monad.State.Lazy
import           Data.Bifunctor               (Bifunctor (first))

import           Control.Applicative          ((<|>))
import           Data.List                    (elemIndex, find, findIndex)
import qualified Data.Map                     as Map
import           Data.Maybe                   (fromJust, fromMaybe)
import qualified Data.Set                     as Set
import           Saga.AST.Syntax              (Expr (..), Name (..), Term (..))

import           Debug.Trace                  (trace, traceM)
import           Saga.AST.TypeSystem.BuiltIns (operatorFnTypes)
import           Saga.AST.TypeSystem.Kinds    (Kind (KConstraint, KConstructor, KProtocol, KType, KVar))

import           Saga.AST.TypeSystem.Types
import qualified Saga.Lexer.Lexer             as L
import           Saga.Parser.Parser           (runSagaExpr, runSagaType)



type Infer a = StateT (Env a) (Except (TypeError a))

data Env a = Env {
    expressions :: Map.Map String (Type a),
    typeVars    :: Map.Map String (Type a),
    typeKinds   :: Map.Map String (Kind a),
    tcount      :: Int,
    kcount      :: Int
} deriving (Show)

initEnv :: Env a
initEnv =  Env {
    expressions = Map.fromList [], --operatorFnTypes,
    typeVars = Map.empty,
    typeKinds = Map.empty,
    tcount = 0,
    kcount = 0
    }

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: a -> Infer a (Type a)
fresh info = do
  s <- get
  let id = Name info $ "t" ++ (letters !! tcount s)
  put s{tcount = tcount s + 1}
  return $ TVar id

fresh_k :: Infer a (Kind a)
fresh_k = do
  s <- get
  let id = "k" ++ (letters !! kcount s)
  put s{tcount = kcount s + 1}
  return $ KVar id



data TypeError a
  = TypeMismatch (Type a) (Type a)
  | WrongType (Expr a) (Type a)
  | WrongKind (Type a) (Kind a)
  | UnknownType String
  | UnexpectedType String
  | UnificationError (Type a) (Type a)
  deriving (Show, Eq)

run :: Show a => Infer a b -> Either String b
run = runInEnv Nothing

runInEnv :: Show a => Maybe (Env a) -> Infer a b -> Either String b
runInEnv env check = show `first` check'
  where check' = runExcept $ evalStateT check (fromMaybe initEnv env)


doInEnv :: Show a => Maybe (Env a) -> Infer a b -> Either String (b, Env a)
doInEnv env check = show `first` check'
  where check' = runExcept $ runStateT check (fromMaybe initEnv env)

infer :: String -> Either String (Type L.Range)
infer input = do
    parsed <- runSagaExpr input
    show `first` runExcept (infer' parsed)
        where
            infer' :: (Eq a, Show a) => Expr a -> Except (TypeError a) (Type a)
            infer' expr = evalStateT (typeof expr) initEnv

inferKind :: String -> Either String (Kind L.Range)
inferKind input = do
    parsed <- runSagaType input
    show `first` runExcept (infer' parsed)
        where
            infer' :: (Eq a, Show a) => TypeExpr a -> Except (TypeError a) (Kind a)
            infer' tyExpr = evalStateT (kindOf tyExpr) initEnv


tyLookup :: Name a -> Infer a (Type a)
tyLookup (Name info id) = do
    env <- get
    maybe (fresh info) return $ Map.lookup id $ typeVars env





typeof :: (Eq a, Show a) => Expr a -> Infer a (Type a)
typeof a = do
    env <- get
    traceM $ "\nENVIRONMENT: " ++ show env
    typeof' a

    where
        typeof' :: (Eq a, Show a) => Expr a -> Infer a (Type a)
        typeof' a | trace ("TYPEOF' " ++ show a ) False = undefined
        typeof' (Term t) = typeof_term t
        typeof' (Assign _ expr) = typeof expr

        typeof' (Clause _ _ expr) = typeof expr
        typeof' (Block _ []) = return TUnit
        typeof' (Block _ exprs) = typeof $ last exprs
        typeof' (Return _ expr) = typeof expr

        typeof' (Parens _ expr) = typeof expr

        typeof' (Identifier (Name info id)) = do
            env <- get
            case Map.lookup id $ expressions env of
                Just ty -> return ty
                Nothing -> do
                    ty <- fresh info
                    modify $ \s -> s{ expressions = Map.insert id ty $ expressions env }
                    return ty

        typeof' (FieldAccess _ recExpr path) = do
            ty <- typeof recExpr
            typeof'' ty path
            where
                typeof'' :: (Eq a, Show a) => Type a -> [Name a] -> Infer a (Type a)
                typeof'' ty [] = return ty
                typeof'' (TRecord _ pairs) (id:rest) =
                    let found = find (\(name, typeExpr) -> name == id) pairs in
                    case found of
                        Just (_, tyExpr) -> do
                            ty <- reduce tyExpr
                            typeof'' ty rest
                        Nothing -> throwError $ UnknownType $ "No field: " <> show id

                typeof'' ty _ = throwError $ UnexpectedType $ "Non record type: " <> show ty

        typeof' (IfElse rt cond true false) = do
            condTy <- typeof cond
            trueTy <- typeof true
            falseTy <- typeof false
            isBool <- unify condTy (TPrimitive rt TBool)
            if not isBool
                then throwError $ TypeMismatch condTy (TPrimitive rt TBool)
                else do
                    match <- unify trueTy falseTy
                    if not match
                        then throwError $ TypeMismatch trueTy falseTy
                        else return trueTy

        typeof' (Lambda rt args body) = do
            (args', env') <- runScoped $ mapM typeofArg args
            bodyTy <- lift $ evalStateT (typeof body) env'
            return $ foldr arrow bodyTy args'
                where
                    typeofArg = typeof . Identifier
                    arrow arg ty = TArrow rt (Type arg) (Type ty)


        typeof' (FnApp rt fnExpr args) = do
            ty  <- typeof fnExpr
            -- | run another state computation to correctly scope the function arguments
            evalScoped $ apply' ty args



            where
                apply' a b | trace ("FN APPLY:\n\t" <> show a <> "\n\t" <> show b) False = undefined
                apply' (TArrow _ argTyExp resultTyExpr) (argExp:rest) = do
                    ty <- typeof argExp
                    argTy <- reduce argTyExp
                    match <- unify ty argTy
                    if not match
                        then throwError $ TypeMismatch ty argTy
                        else do
                            r <- reduce resultTyExpr
                            evalScoped $ apply' r rest

                apply' ty@(TVar (Name info id)) (argExpr:rest) = do
                    env <- get
                    case Map.lookup id $ typeVars env of
                        Just f@(TArrow {}) -> evalScoped $ do
                            unify ty f
                            apply' f (argExpr:rest)
                        Just ty             -> throwError $ UnexpectedType $ "Non arrow type: " <> show ty
                        Nothing             -> do
                            tyArg <- typeof argExpr
                            out <- fresh info
                            let fn = TArrow info (Type tyArg) (Type out)
                            unify ty fn
                            evalScoped $ apply' fn (argExpr:rest)


                apply' ty@(TVar (Name _ id)) [] = do
                    env <- get
                    case Map.lookup id $ typeVars env of
                        Just ty' -> return ty'
                        Nothing  -> return ty
                apply' ty [] = return ty


                apply' ty _ = throwError $ UnexpectedType $ "Non arrow type: " <> show ty




typeof_term :: (Eq a, Show a) => Term a -> Infer a (Type a)
typeof_term term = case term of
    (LTuple rt tuple) -> do
        ttuple <- mapM expanded tuple
        let tyExprs = Type <$> ttuple
        return $ TTuple rt tyExprs

    (LRecord rt pairs)  -> do
        tyPairs <- mapM tyExpr pairs
        return $ TRecord rt tyPairs
        where
            tyExpr (name, expr) = do
                ty <- expanded expr
                return (name, Type ty)

    (LList rt list) -> do
        t <- tyArgs list
        return $ TParametric (Name rt "List") (Type t)
            where
                tyArgs [] = fresh rt
                tyArgs [expr] = expanded expr
                tyArgs (expr:rest) = do
                    ty <- expanded expr
                    tys <- mapM expanded rest

                    vals <- mapM (unify ty) tys
                    let mismatch = elemIndex False vals

                    case mismatch of
                        Just i  -> throwError $ TypeMismatch ty (tys !! i)
                        Nothing -> return ty

    lit -> return $ TLiteral lit
    where
        expanded :: (Eq a, Show a ) => Expr a -> Infer a (Type a)
        expanded = typeof >=> expand


reduce :: Show a => TypeExpr a -> Infer a (Type a)
reduce a | trace ("reduce: " ++ show a) False = undefined
reduce (Type ty)                        = return ty
reduce (TParens _ tyExp)                = reduce tyExp
reduce (TClause _ _ tyExp)              = reduce tyExp
reduce (TBlock _ [])                    = return TUnit
reduce (TBlock _ tyExps)                = reduce $ last tyExps
reduce (TReturn _ tyExp)                = reduce tyExp
reduce (TConditional _ cond true false) = reduce true -- assumes same return type, will change with union types
reduce (TFnApp info fnExpr argExprs)    = do
    constructor <- reduce fnExpr
    traceM $ "Fn Expression: " <> show constructor


    case (constructor, argExprs) of
        (TVar t, []) -> evalScoped $ tyLookup  t
        (TVar t, [last]) -> return $ TParametric t last
        (TVar t@(Name info id), tyExpr:tail) -> do
            out <- evalScoped $ reduce $ TFnApp info tyExpr tail
            return $ TParametric t (Type out)

        (TParametric _ body, []) -> reduce body
        (TParametric (Name _ arg) body, [tyExpr]) -> do
            (_, env') <- runScoped $ updateEnv arg tyExpr
            lift $ evalStateT (reduce body) env'
        (TParametric (Name _ arg) body, tyExpr:tail) -> do
            (_, env') <- runScoped $ updateEnv arg tyExpr
            lift $ evalStateT (reduce $ TFnApp info body tail) env'

        _ -> throwError $ UnexpectedType "Cannot apply this type expression"

    where
      updateEnv arg tyExpr = do
        env' <- get
        ty <- reduce tyExpr
        let tyVars = Map.insert arg ty (typeVars env')
        modify $ \s -> s{ typeVars = Map.union tyVars $ typeVars s }

reduce (TLambda _ args body) = return $ fn args
    where
        fn [id]      = TParametric id body
        fn (id:tail) = TParametric id $ Type (fn tail)





unify :: (Eq a, Show a) => Type a -> Type a -> Infer a Bool
unify a b | trace ("unify: " ++ show a ++ " WITH: " ++ show b) False = undefined
unify t1 t2 = case (t1, t2) of
    (TVar (Name _ id), _) -> unify' id t2
    (_, TVar (Name _ id)) -> unify' id t1
    _                     ->
        if t1 == t2
            then return True
            else do
                match <- t1 `isSubtype` t2
                if not match
                    then throwError $ UnificationError t1 t2
                    else return match
    where
        unify' :: (Eq a, Show a) => String -> Type a -> Infer a Bool
        unify' id' ty = do
            env <- get
            case Map.lookup id' $ typeVars env of
                Nothing -> do
                    modify $ \s -> s{ typeVars = Map.insert id' ty $ typeVars s }
                    return True
                Just ty' -> return $ ty == ty'


isSubtype ::(Eq a, Show a) => Type a -> Type a -> Infer a Bool
isSubtype  a b | trace ("subtype " ++ show a ++ " <: " ++ show b ++ "\n  ") False = undefined
sub `isSubtype` parent = do
    env <- get
    case (sub, parent) of

        (TLiteral (LInt _ _), TPrimitive _ TInt)       -> return True
        (TLiteral (LString _ _), TPrimitive _ TString) -> return True
        (TLiteral (LBool _ _), TPrimitive _ TBool)     -> return True

        (TPrimitive _ prim1, TPrimitive _ prim2)       -> return $ prim1 == prim2

        (TTuple _ tup1, TTuple _ tup2)  -> do
            tup1' <- mapM reduce tup1
            tup2' <- mapM reduce tup2
            allTrue <$> zipWithM isSubtype tup1' tup2'

        (TRecord _ pairs1, TRecord _ pairs2)  -> do
            pairs1' <- mapM (mapM reduce) pairs1
            pairs2' <- mapM (mapM reduce) pairs2
            let check (name, ty2) = case lookup name pairs1' of
                    Nothing  -> return False
                    Just ty1 -> ty1 `isSubtype` ty2

            allTrue <$> mapM check pairs2'

        (TParametric arg1 out1, TParametric arg2 out2) -> do
            arg1' <- tyLookup arg1
            arg2' <- tyLookup arg2
            out1' <- reduce out1
            out2' <- reduce out2
            (out', env') <- runScoped $ out1' `isSubtype` out2'
            arg' <- lift $ evalStateT (arg1' `isSubtype` arg2') env'
            return $ out' && arg'

        (TArrow _ input1 output1, TArrow _ input2 output2) -> do
            input1' <- reduce input1
            input2' <- reduce input2
            (input', env') <- runScoped $ input1' `isSubtype` input2'
            output1' <- reduce output1
            output2' <- reduce output2

            output' <- lift $ evalStateT (output1' `isSubtype`  output2') env'
            return $ input' && output'

        (TIdentifier (Name _ id), TIdentifier (Name _ id'))
            | id == id' -> return True
            | Just ty  <- Map.lookup id  $ typeVars env
            , Just ty' <- Map.lookup id' $ typeVars env
                -> ty `isSubtype` ty'
            | otherwise -> return False

        (TIdentifier (Name _ id), _)
            | Just ty  <- Map.lookup id  $ typeVars env
                -> ty `isSubtype` parent
            | otherwise -> return False

        (TVar (Name _ id), TVar (Name _ id')) -> return $ id == id'

        (ty@(TVar (Name info id)), _)
            | Just ty <- Map.lookup id $ typeVars env
                -> ty `isSubtype` parent
            | otherwise -> do
                modify $ \s -> s{ typeVars = Map.insert id parent $ typeVars s }
                return True

        (_, _) -> return False

    where
        allTrue :: Foldable t => t Bool -> Bool
        allTrue = and



expand :: (Eq a, Show a) => Type a -> Infer a (Type a)
expand (TLiteral (LInt rt _))    = return $ TPrimitive rt TInt
expand (TLiteral (LBool rt _))   = return $ TPrimitive rt TBool
expand (TLiteral (LString rt _)) = return $ TPrimitive rt TString
expand (TLiteral termTy)         = typeof_term termTy
expand ty                        = return ty


unknown :: String -> TypeError a
unknown id = UnknownType $  "Unknown type \"" <> id <> "\""




kindOf :: Show a => TypeExpr a -> Infer a (Kind a)
kindOf a | trace ("kind of: " ++ show a) False = undefined

kindOf tyExpr = do
    env <- get
    ty <- reduce tyExpr
    case ty of
        TProtocol _ _ -> return KProtocol
        TConstrained {} -> return KConstraint

        TParametric p@(Name info param) out -> do
            case Map.lookup param (typeVars env) of
                Just ty -> kindOf (Type ty)
                Nothing -> evalScoped $ build p out

        TVar (Name _ id) -> maybe fresh_k return $ Map.lookup id $ typeKinds env
        _ -> return KType

    where
        build arg@(Name _ id) out  = do
            let ty' = TVar arg
            env <- get
            k <- kindOf (Type ty')
            traceM $ "Inferred kind: " ++ show k
            let typeVars' = Map.insert id ty' $ typeVars env
            modify $ \s -> s{ typeVars = Map.union typeVars' $ typeVars s}
            out' <- kindOf out
            traceM $ "Inferred out kind: " ++ show out'
            return $ KConstructor k out'




runScoped :: (MonadState s (t m), MonadTrans t, Monad m) => StateT s m a -> t m (a, s)
runScoped action = do
    env <- get
    lift $ runStateT action env

evalScoped :: (MonadState s (t m), MonadTrans t, Monad m) => StateT s m b -> t m b
evalScoped action = do
    env <- get
    lift $ evalStateT action env
