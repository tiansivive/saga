module Saga.AST.Subtyping where

import           Control.Monad            (zipWithM)
import           Data.Maybe               (fromMaybe, isJust)
import           Saga.AST.Inference
import           Saga.AST.Syntax

import           Control.Monad.State.Lazy
import qualified Data.Map                 as Map
import           Debug.Trace              (trace, traceM)



isSubtype ::(Eq a, Show a) => Type a -> Type a -> Infer a Bool
isSubtype  a b | trace ("subtype " ++ (show a) ++ " <: " ++ (show b) ++ "\n  ") False = undefined
sub `isSubtype` parent = case (sub, parent) of


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

    (TArrow _ input1 output1, TArrow _ input2 output2) -> do
        input' <- reduce input1 `isSubtype` reduce input2
        output' <- reduce output1 `isSubtype` reduce output2
        return $ input' && output'


    (TVar (Name _ id), TVar (Name _ id')) -> return $ id == id'

    (TVar (Name info id), _) -> do
        env <- get
        case Map.lookup id $ typeVars env of
            Just ty -> ty `isSubtype` parent
            Nothing -> do
                put env{ typeVars = Map.insert id parent $ typeVars env }
                return True

    (_, _) -> return False
    where
        allTrue :: Foldable t => t Bool -> Bool
        allTrue = and







