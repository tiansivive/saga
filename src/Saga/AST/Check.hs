module Saga.AST.Check where


import qualified Saga.AST.Inference       as Infer
import           Saga.AST.Inference       (Infer)
import           Saga.AST.Subtyping
import           Saga.AST.Syntax

import           Control.Monad.State.Lazy
import qualified Data.Map                 as Map






check :: (Eq a, Show a) => Expr a -> TypeExpr a -> Infer a Bool
check e@(Term term) ty = checkTerm term ty


-- check e@(Identifier _) ty =  do
--     inferred <- Infer.typeof e
--     inferred `isSubtype` ty
-- check (Assign name e) ty =  do
--     inferred <- Infer.typeof e
--     inferred `isSubtype` ty
-- check expr ty       =  return False

check expr ty       = case Infer.reduce ty of
    ty'@(TPolymorphic _) -> do
        inferred <- Infer.typeof expr
        ty' `isSubtype` inferred
    _ -> do
        inferred <- Infer.typeof expr
        inferred `isSubtype` Infer.reduce ty


checkTerm ::(Eq a, Show a) => Term a -> TypeExpr a -> Infer a Bool
checkTerm  = unify . Term




unify :: (Eq a, Show a) => Expr a -> TypeExpr a -> Infer a Bool
unify expr ty = unify' expr (Infer.reduce ty)
    where
        unify' :: (Eq a, Show a) => Expr a -> Type a -> Infer a Bool

        unify' expr ty@(TPolymorphic (Name _ id)) = do
            env <- get
            inferred <- Infer.typeof expr
            let tys = Infer.identifiers env
            case Map.lookup id tys of
                Just ty -> inferred `isSubtype` ty
                Nothing -> do
                    put env{Infer.identifiers = Map.insert id inferred tys }
                    return True



fn :: a -> (a, Int)
fn x = (x, 1)

foo = fn 1


fn2 :: String -> (String, Int)
fn2 = fn
