module Saga.AST.Subtyping where

import           Control.Monad            (zipWithM)
import           Data.Maybe               (fromMaybe, isJust)
import           Saga.AST.Inference
import           Saga.AST.Syntax

import           Control.Monad.State.Lazy
import qualified Data.Map                 as Map
import           Debug.Trace              (trace, traceM)



isSubtype ::(Eq a, Show a) => Type a -> Type a -> Infer a Bool
sub `isSubtype` parent = case (sub, parent) of

    (TIdentifier (Name info id), _) -> do
        env <- get
        poly <- fresh info
        let sub' = fromMaybe poly $ Map.lookup id $ identifiers env
        sub' `isSubtype` parent


    (TLiteral (LInt _ _), TPrimitive _ TInt)       -> return True
    (TLiteral (LString _ _), TPrimitive _ TString) -> return True
    (TLiteral (LBool _ _), TPrimitive _ TBool)     -> return True

    (TPrimitive _ prim1, TPrimitive _ prim2)       -> return $ prim1 == prim2

    (TTuple _ tup1, TTuple _ tup2)  -> let
        tup1' = reduce <$> tup1
        tup2' = reduce <$> tup2
        in allTrue <$> zipWithM isSubtype tup1' tup2'

    (TRecord _ pairs1, TRecord _ pairs2)  -> let
        pairs1' = fmap reduce <$> pairs1
        pairs2' = fmap reduce <$> pairs2
        check (name, ty2) = case lookup name pairs1' of
            Nothing  -> return False
            Just ty1 -> isSubtype ty1 ty2

        in allTrue <$> mapM check pairs2'

    (TParametric cons1 arg1, TParametric cons2 arg2) -> do
        cons' <- reduce cons1 `isSubtype` reduce cons2
        arg'  <- reduce arg1  `isSubtype` reduce arg2
        return $ cons' && arg'

    (ty@(TPolymorphic (Name info id)), _) -> do
        env <- get
        case Map.lookup id $ identifiers env of
            Nothing -> do
                put env{ identifiers = Map.insert id ty $ identifiers env }
                return True
            Just (TPolymorphic _) -> return True
            Just ty' -> ty' `isSubtype` parent

    where
        allTrue :: Foldable t => t Bool -> Bool
        allTrue = and







