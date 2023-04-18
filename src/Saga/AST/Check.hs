module Saga.AST.Check where


import qualified Saga.AST.Inference       as Infer
import           Saga.AST.Inference       (Infer)
import           Saga.AST.Subtyping
import           Saga.AST.Syntax

import           Control.Monad.State.Lazy
import qualified Data.Map                 as Map






check :: (Eq a, Show a) => Expr a -> TypeExpr a -> Infer a Bool
check expr ty = do
    inferred <- Infer.typeof expr
    inferred `isSubtype` Infer.reduce ty

-- check (Identifier (Name _ x)) tyExpr =
--     let
--         ty = Infer.reduce tyExpr
--     in do
--         env <- get
--         case Map.lookup x $ Infer.expressions env  of
--             Just ty' -> unify ty ty'
--             Nothing  -> return False

-- check (FnApp _ fn args) ty = do
--   fn' <- Infer.typeof fn
--   args' <- mapM Infer.typeof args
--   unify t1 (ArrowType t2 t)
--   check r env e2 (substitute r t2)
-- check expr ty = check' expr $ Infer.reduce ty
--     where
--         check' (Identifier name) t = do
--             inferred <- Infer.typeof expr
--             inferred `isSubtype` t


-- check e@(Identifier _) ty =  do
--     inferred <- Infer.typeof e
--     inferred `isSubtype` ty
-- check (Assign name e) ty =  do
--     inferred <- Infer.typeof e
--     inferred `isSubtype` ty
-- check expr ty       =  return False
-- check expr ty          = unify expr ty
-- check expr ty       = case Infer.reduce ty of
--     ty'@(TVar _) -> do
--         inferred <- Infer.typeof expr
--         ty' `isSubtype` inferred
--     _ -> do
--         inferred <- Infer.typeof expr
--         inferred `isSubtype` Infer.reduce ty


-- unify :: Type a -> Type a -> Infer a Bool
-- unify = _unify

-- checkTerm ::(Eq a, Show a) => Term a -> TypeExpr a -> Infer a Bool
-- checkTerm  = unify . Term




-- unify :: (Eq a, Show a) => Expr a -> TypeExpr a -> Infer a Bool
-- unify expr ty = unify' expr (Infer.reduce ty)
--     where
--         unify' :: (Eq a, Show a) => Expr a -> Type a -> Infer a Bool

--         unify' expr ty@(TVar (Name _ id)) = do
--             env <- get
--             inferred <- Infer.typeof expr
--             let tys = Infer.expressions env
--             case Map.lookup id tys of
--                 Just ty -> inferred `isSubtype` ty
--                 Nothing -> do
--                     put env{Infer.expressions = Map.insert id inferred tys }
--                     return True

--         unify' expr ty = do
--             inferred <- Infer.typeof expr
--             inferred `isSubtype` ty



-- type SubtypeRelation = forall a. [(Type a, Type a)]

-- unify :: SubtypeRelation -> Type a -> Type a -> Maybe ()
-- unify r ty@(TVar _ x) t = case lookup ty r of
--     Just t' -> unify r t' t
--     Nothing -> if t == VarType x then Just () else Just $ [(VarType x, t)]

-- unify r t ty@(TVar _ x)  = unify r ty t
-- unify r (TArrow _ t1 t2) (TArrow _ t1' t2') = do
--     unify r t1 t1'
--     unify r t2 t2'
-- -- unify r (ForallType x t) (ForallType y t') | x == y = unify r t t'
-- unify r t1 t2
--     | t1 == t2 = Just ()
--     | otherwise = Nothing
