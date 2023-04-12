{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}


module Saga.AST.Inference where

import           Control.Monad.Except
import           Control.Monad.Identity   (Identity (runIdentity))
import           Control.Monad.State.Lazy
import           Data.Bifunctor           (Bifunctor (first))

import           Control.Applicative      ((<|>))
import           Data.List
import qualified Data.Map                 as Map
import           Data.Maybe               (fromJust, fromMaybe)
import qualified Data.Set                 as Set
import           Saga.AST.Syntax
import qualified Saga.Lexer.Lexer         as L
import           Saga.Parser.Parser       (runSagaExpr)


type Infer a = StateT (Env a) (Except (TypeError a))

data Env a = Env {
    identifiers :: Map.Map String (Type a),
    count       :: Int
} deriving (Show)

initEnv :: Env a
initEnv =  Env { identifiers = Map.empty, count = 0 }

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: a -> Infer a (Type a)
fresh info = do
  s <- get
  put s{count = count s + 1}
  return $ TPolymorphic $ Name info (letters !! count s)



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
typeof (Block _ []) = return TVoid
typeof (Block _ exprs) = typeof $ last exprs
typeof (Return _ expr) = typeof expr

typeof (Parens _ expr) = typeof expr

typeof (Identifier (Name rt id)) = do
    env <- get
    case Map.lookup id $ identifiers env of
        Just ty -> return ty
        Nothing -> return $ TPolymorphic (Name rt "a")

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
                builtInList = Type $ TIdentifier $ Name rt "List"

                tyArgs [] = return $ TPolymorphic (Name rt "a")
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
reduce (TBlock _ [])                    = TVoid
reduce (TBlock _ tyExps)                = reduce $ last tyExps
reduce (TReturn _ tyExp)                = reduce tyExp
reduce (TConditional _ cond true false) = reduce true -- assumes same return type, will change with union types
reduce (TLambda _ args body)            = reduce body
reduce (TFnApp _ fnExpr args)           = reduce fnExpr



expand :: (Eq a, Show a) => Type a -> Infer a (Type a)
expand (TLiteral (LInt rt _))    = return $ TPrimitive rt TInt
expand (TLiteral (LBool rt _))   = return $ TPrimitive rt TBool
expand (TLiteral (LString rt _)) = return $ TPrimitive rt TString
expand (TLiteral termTy)         = typeof_term termTy
expand ty                        = return ty


unknown :: String -> TypeError a
unknown id = UnknownType $  "Unknown type \"" <> id <> "\""
