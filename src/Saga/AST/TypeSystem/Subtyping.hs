module Saga.AST.TypeSystem.Subtyping where

import           Control.Monad            (zipWithM)
import           Data.Maybe               (fromMaybe, isJust)
import           Saga.AST.TypeSystem.Inference
import           Saga.AST.TypeSystem.Types
import           Saga.AST.Syntax

import           Control.Monad.State.Lazy
import qualified Data.Map                 as Map
import           Debug.Trace              (trace, traceM)



isSubtype ::(Eq a, Show a) => Type a -> Type a -> Infer a Bool
--isSubtype  a b | trace ("subtype " ++ (show a) ++ " <: " ++ (show b) ++ "\n  ") False = undefined
sub `isSubtype` parent = do
    env <- get
    case (sub, parent) of

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
                Just ty1 -> ty1 `isSubtype` ty2

            in allTrue <$> mapM check pairs2'

        (TParametric cons1 args1, TParametric cons2 args2) -> 
            let 
                args1' = reduce <$> args1
                args2' = reduce <$> args2
            in do
                cons' <- reduce cons1 `isSubtype` reduce cons2
                args' <- allTrue <$> zipWithM isSubtype args1' args2'
                return $ cons' && args'

        (TArrow _ input1 output1, TArrow _ input2 output2) -> do
            input' <- reduce input1 `isSubtype` reduce input2
            output' <- reduce output1 `isSubtype` reduce output2
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
                put env{ typeVars = Map.insert id parent $ typeVars env }
                return True

        (_, _) -> return False

    where
        allTrue :: Foldable t => t Bool -> Bool
        allTrue = and







