{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}


module Saga.AST.TypeCheck where

import           Control.Monad.Except
import           Control.Monad.Identity   (Identity (runIdentity))
import           Control.Monad.State.Lazy
import           Data.Bifunctor           (Bifunctor (first))

import           Data.List
import qualified Data.Map                 as Map
import           Saga.AST.Syntax
import qualified Saga.Lexer.Lexer         as L
import           Saga.Parser.Parser       (runSagaExpr)
import           Saga.Utils.Utils

type Env a = Map.Map String (Type a)

type TypeCheck a = StateT (Env a) (Except (TypeError a)) (Type a)


data TypeError a
  = TypeMismatch (Type a) (Type a)
  | UnknownType String
  | UnexpectedType String
  deriving (Show, Eq)



infer :: String -> Either String (Type L.Range)
infer input = do
    parsed <- runSagaExpr input
    show `first` runExcept (infer' parsed)
        where
            infer' :: (Eq a, Show a) => Expr a -> Except (TypeError a) (Type a)
            infer' expr = evalStateT (typeof expr) Map.empty



unify :: Type a -> Type a -> TypeCheck a
unify (TLiteral (LInt _ _)) (TPrimitive rt TInt) = return $ TPrimitive rt TInt
unify (TLiteral (LBool _ _)) (TPrimitive rt TBool) = return $ TPrimitive rt TBool
unify (TLiteral (LString _ _)) (TPrimitive rt TString) = return $ TPrimitive rt TString

-- | TODO: change result to union type when they're implemented
unify (TLiteral (LString _ _)) (TLiteral (LString rt _)) = return $ TPrimitive rt TString
unify (TLiteral (LInt _ _)) (TLiteral (LInt rt _)) = return $ TPrimitive rt TInt
unify (TLiteral (LBool _ _)) (TLiteral (LBool rt _)) = return $ TPrimitive rt TBool

unify (TPolymorphic _) ty = return ty
unify ty (TPolymorphic _) = return ty





typeof :: (Eq a, Show a) => Expr a -> TypeCheck a
typeof (Term t) = typeof_term t
typeof (Assign _ expr) = typeof expr

typeof (Clause _ _ expr) = typeof expr
typeof (Block _ []) = return TVoid
typeof (Block _ exprs) = typeof $ last exprs
typeof (Return _ expr) = typeof expr

typeof (Parens _ expr) = typeof expr

typeof (Identifier (Name rt id)) = do
    env <- get
    case Map.lookup id env of
        Just ty -> return ty
        Nothing -> return $ TPolymorphic (Name rt "a")

typeof (FieldAccess _ recExpr path) = do
    ty <- typeof recExpr
    typeof' ty path
    where
        typeof' :: (Eq a, Show a) => Type a -> [Name a] -> TypeCheck a
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




typeof_term :: (Eq a, Show a) => Term a -> TypeCheck a
typeof_term (LTuple rt tuple) = do
    ttuple <- mapM (typeof >=> narrow) tuple
    let tyExprs = Type <$> ttuple
    return $ TTuple rt tyExprs

typeof_term (LRecord rt pairs)  = do
    tyPairs <- mapM tyExpr pairs
    return $ TRecord rt tyPairs
    where
        tyExpr (name, expr) = do
            ty <- narrow <=< typeof $ expr
            return (name, Type ty)

typeof_term (LList rt list) = do
    t <- tyArgs list
    return $ TParametric builtInList (Type t)
        where
            narrowed :: (Eq a, Show a ) => Expr a -> TypeCheck a
            narrowed = typeof >=> narrow

            builtInList = Type $ TIdentifier $ Name rt "List"

            tyArgs [] = return $ TPolymorphic (Name rt "a")
            tyArgs [expr] = narrowed expr
            tyArgs (expr:rest) = do
                ty <- narrowed expr
                tys <- mapM narrowed rest

                let mismatch = find (/= ty) tys
                case mismatch of
                    Just ty' -> throwError $ TypeMismatch ty ty'
                    Nothing  -> return ty

typeof_term lit               = return $ TLiteral lit



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


narrow :: (Eq a, Show a) => Type a -> TypeCheck a
narrow (TLiteral (LInt rt _))    = return $ TPrimitive rt TInt
narrow (TLiteral (LBool rt _))   = return $ TPrimitive rt TBool
narrow (TLiteral (LString rt _)) = return $ TPrimitive rt TString
narrow (TLiteral termTy)         = typeof_term termTy
narrow ty                        = return ty


unknown :: String -> TypeError a
unknown id = UnknownType $  "Unknown type \"" <> id <> "\""

