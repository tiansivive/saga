{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}


module Saga.AST.TypeSystem.Inference where

import           Control.Monad.Except
import           Control.Monad.Identity   (Identity (runIdentity))
import           Control.Monad.State.Lazy
import           Data.Bifunctor           (Bifunctor (first))

import           Control.Applicative      ((<|>))
import           Data.List
import qualified Data.Map                 as Map
import           Data.Maybe               (fromJust, fromMaybe)
import qualified Data.Set                 as Set
import           Saga.AST.Syntax          (
                                           Expr (..), Name (..), Term(..)
                                           
                                       )
import           Saga.AST.TypeSystem.Types
import qualified Saga.Lexer.Lexer         as L
import           Saga.Parser.Parser       (runSagaExpr)


type Infer a = StateT (Env a) (Except (TypeError a))

data Env a = Env {
    expressions :: Map.Map String (Type a),
    typeVars    :: Map.Map String (Type a),
    count       :: Int
} deriving (Show)

initEnv :: Env a
initEnv =  Env { expressions = Map.empty, typeVars = Map.empty, count = 0 }

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: a -> Infer a (Type a)
fresh info = do
  s <- get
  put s{count = count s + 1}
  let id = Name info (letters !! count s)
  return $ TVar id



data TypeError a
  = TypeMismatch (Type a) (Type a)
  | UnknownType String
  | UnexpectedType String
  deriving (Show, Eq)

run :: Show a => Infer a Bool -> Either String Bool
run = runInEnv Nothing

runInEnv :: Show a => Maybe (Env a) -> Infer a Bool -> Either String Bool
runInEnv env check = show `first` check'
  where check' = runExcept $ evalStateT check (fromMaybe initEnv env)

infer :: String -> Either String (Type L.Range)
infer input = do
    parsed <- runSagaExpr input
    show `first` runExcept (infer' parsed)
        where
            infer' :: (Eq a, Show a) => Expr a -> Except (TypeError a) (Type a)
            infer' expr = evalStateT (typeof expr) initEnv


typeof :: (Eq a, Show a) => Expr a -> Infer a (Type a)
typeof (Term t) = typeof_term t
typeof (Assign _ expr) = typeof expr

typeof (Clause _ _ expr) = typeof expr
typeof (Block _ []) = return TUnit
typeof (Block _ exprs) = typeof $ last exprs
typeof (Return _ expr) = typeof expr

typeof (Parens _ expr) = typeof expr

typeof (Identifier (Name info id)) = do
    env <- get
    case Map.lookup id $ expressions env of
        Just ty -> return ty
        Nothing -> do
            ty <- fresh info
            put env{ expressions = Map.insert id ty $ expressions env }
            return ty

typeof (FieldAccess _ recExpr path) = do
    ty <- typeof recExpr
    typeof' ty path
    where
        typeof' :: (Eq a, Show a) => Type a -> [Name a] -> Infer a (Type a)
        typeof' ty [] = return ty
        typeof' (TRecord _ pairs) (id:rest) =
            let found = find (\(name, typeExpr) -> name == id) pairs in
            case found of
                Just (_, tyExpr) -> typeof' (reduce tyExpr) rest
                Nothing -> throwError $ UnknownType $ "No field: " <> show id

        typeof' ty _ = throwError $ UnexpectedType $ "Non record type: " <> show ty



typeof (IfElse rt cond true false) = do
    condTy <- typeof cond
    trueTy <- typeof true
    falseTy <- typeof false
    if condTy /= TPrimitive rt TBool
        then throwError $ TypeMismatch condTy (TPrimitive rt TBool)
        else if trueTy /= falseTy
            then throwError $ TypeMismatch trueTy falseTy
            else return trueTy

typeof (Lambda rt args body) = do
    bodyTy <- typeof body
    args' <- mapM typeofArg args
    return $ foldr arrow bodyTy args'
        where
            typeofArg = typeof . Identifier
            arrow arg ty = TArrow rt (Type arg) (Type ty)


typeof (FnApp rt fnExpr args) = do
    ty  <- typeof fnExpr
    typeof' ty args

    where
        typeof' ty [] = return ty
        typeof' (TArrow _ argTyExp result) (argExp:rest) = do
            ty <- typeof argExp
            let argTy = reduce argTyExp
            if argTy /= ty
                then throwError $ TypeMismatch argTy ty
                else typeof' (reduce result) rest

        typeof' ty _ = throwError $ UnexpectedType $ "Non arrow type: " <> show ty




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
        return $ TParametric builtInList (Type t)
            where
                builtInList = Type $ TVar $ Name rt "List"

                tyArgs [] = return $ TVar (Name rt "a")
                tyArgs [expr] = expanded expr
                tyArgs (expr:rest) = do
                    ty <- expanded expr
                    tys <- mapM expanded rest

                    let mismatch = find (/= ty) tys
                    case mismatch of
                        Just ty' -> throwError $ TypeMismatch ty ty'
                        Nothing  -> return ty

    lit -> return $ TLiteral lit
    where
        expanded :: (Eq a, Show a ) => Expr a -> Infer a (Type a)
        expanded = typeof >=> expand



reduce :: TypeExpr a -> Type a
reduce (Type ty)                        = ty
reduce (TParens _ tyExp)                = reduce tyExp
reduce (TClause _ _ tyExp)              = reduce tyExp
reduce (TBlock _ [])                    = TUnit
reduce (TBlock _ tyExps)                = reduce $ last tyExps
reduce (TReturn _ tyExp)                = reduce tyExp
reduce (TConditional _ cond true false) = reduce true -- assumes same return type, will change with union types
reduce (TFnApp _ fnExpr args)           = returnType fnExpr 0
    where
        returnType ty i
            | i == length args -1 = reduce ty
            | otherwise = case reduce ty of
                (TArrow _ _ return) -> returnType return (i +1)
                _ -> error "Tried to apply a type other than function"

reduce (TLambda info args body)         = TArrow info (Type args') body
    where
        arrow = TArrow info
        build [] = TUnit
        build [arg] = arg
        build (ty:rest) = foldr (\returnTy argTy -> Type returnTy `arrow` Type argTy) ty rest
        args' = build (TVar <$> args)



expand :: (Eq a, Show a) => Type a -> Infer a (Type a)
expand (TLiteral (LInt rt _))    = return $ TPrimitive rt TInt
expand (TLiteral (LBool rt _))   = return $ TPrimitive rt TBool
expand (TLiteral (LString rt _)) = return $ TPrimitive rt TString
expand (TLiteral termTy)         = typeof_term termTy
expand ty                        = return ty


unknown :: String -> TypeError a
unknown id = UnknownType $  "Unknown type \"" <> id <> "\""
