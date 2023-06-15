{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Saga.AST.TypeSystem.Check where


import           Saga.AST.Syntax
import qualified Saga.AST.TypeSystem.Inference as Infer
import           Saga.AST.TypeSystem.Inference (Infer)
import           Saga.AST.TypeSystem.Kinds
import           Saga.AST.TypeSystem.Subtyping
import           Saga.AST.TypeSystem.Types

import           Control.Monad.State.Lazy
import qualified Data.Map                      as Map






check :: (Eq a, Show a) => Expr a -> TypeExpr a -> Infer a Bool
check expr ty = do
    inferred <- Infer.typeof expr
    ty' <- Infer.reduce ty
    case ty' of
        TVar id -> ty' `isSubtype` inferred
        _       -> inferred `isSubtype` ty'



check_kind :: (Eq a, Show a) => Type a -> Kind a -> Infer a Bool
check_kind (TVar (Name _ id)) k = do
    env <- get
    case Map.lookup id $ Infer.typeKinds env of
        Just k' -> return $ k == k'
        Nothing -> do
            put env{ Infer.typeKinds = Map.insert id k $ Infer.typeKinds env }
            return True

check_kind ty k = do
    inferred <- Infer.kindOf (Type ty)
    return $ inferred == k
