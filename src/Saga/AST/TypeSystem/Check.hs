{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Saga.AST.TypeSystem.Check where


import           Saga.AST.Syntax
import qualified Saga.AST.TypeSystem.Inference as Infer
import           Saga.AST.TypeSystem.Inference (Infer, isSubtype)
import           Saga.AST.TypeSystem.Kinds

import           Saga.AST.TypeSystem.Types

import           Control.Monad.State.Lazy
import qualified Data.Map                      as Map
import           Debug.Trace                   (trace, traceIO, traceM,
                                                traceShow)
import           Foreign                       (ptrToIntPtr)






check :: (Eq a, Show a) => Expr a -> TypeExpr a -> Infer a Bool
--check  a b | trace ("check " ++ show a ++ " AGAINST: " ++ show b ++ "\n  ") False = undefined
check expr ty = do
    inferred <- Infer.typeof expr
    ty' <- Infer.reduce ty
    case ty' of
        TVar id -> ty' `isSubtype` inferred
        _       -> inferred `isSubtype` ty'







check_kind :: (Eq a, Show a) => TypeExpr a -> Kind a -> Infer a Bool
check_kind  a b | trace ("\n---------------\nCHECK KIND:\n" ++ show a ++ "\nAGAINST: " ++ show b ) False = undefined

check_kind tyExpr k = do

    ty <- Infer.reduce tyExpr
    traceM $ "Reduced tyExpr to: " <> show ty
    case ty of
        TVar (Name _ id) -> do
            env <- get
            traceM $ "checking TVar's kind:" <> show ty
            case Map.lookup id $ Infer.typeKinds env of
                Just k' -> return $ k == k'
                Nothing -> do
                    traceM $ "Inserting into type kinds:" <> show k
                    modify $ \s -> s{ Infer.typeKinds = Map.insert id k $ Infer.typeKinds s }
                    return True
        _ -> do
            inferred <- Infer.kindOf (Type ty)
            traceM $ "Check inferred kind: " <> show inferred
            traceM $ "Comparing with kind: " <> show k
            traceM $ "Equality check: " <> show (inferred == k)

            return $ inferred == k







