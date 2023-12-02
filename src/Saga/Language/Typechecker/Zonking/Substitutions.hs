{-# LANGUAGE TypeFamilies #-}
module Saga.Language.Typechecker.Zonking.Substitutions where
import qualified Data.Set                                      as Set
import qualified Saga.Language.Core.Expr                       as AST
import           Saga.Language.Core.Expr                       (Expr)

import           Saga.Language.Typechecker.Solver.Substitution (Substitutable (..))
import           Saga.Language.Typechecker.Type                (PolymorphicVar,
                                                                Type)




instance Substitutable Expr where
    type Target Expr = Type

    apply sub (AST.Typed e ty) = AST.Typed (apply sub e) (apply sub ty)

    apply sub (AST.FnApp fn args) = AST.FnApp (apply sub fn) (fmap (apply sub) args)
    apply sub (AST.Lambda params body) = AST.Lambda params (apply sub body)

    apply sub (AST.List elems) = AST.List $ fmap (apply sub) elems
    apply sub (AST.Tuple elems) = AST.Tuple $ fmap (apply sub) elems
    apply sub (AST.Record pairs) = AST.Record $ fmap (apply sub) pairs

    apply sub (AST.Match scrutinee cases) = AST.Match (apply sub scrutinee) (apply sub <$> cases)

    apply sub (AST.Block stmts) = AST.Block $ apply sub <$> stmts

    apply sub e = e


    ftv (AST.Typed e ty) = ftv e <> ftv ty

    ftv (AST.FnApp fn args) = ftv fn <> foldl (\tvs a -> tvs <> ftv a) Set.empty args
    ftv (AST.Lambda params body) = ftv body

    ftv (AST.List elems) = foldl (\tvs e -> tvs <> ftv e) Set.empty elems
    ftv (AST.Tuple elems) = foldl (\tvs e -> tvs <> ftv e) Set.empty elems
    ftv (AST.Record pairs) = foldl (\tvs p -> tvs <> ftv p) Set.empty pairs

    ftv (AST.Match scrutinee cases) = ftv scrutinee <> foldl (\tvs c -> tvs <> ftv c) Set.empty cases

    ftv (AST.Block stmts) = foldl (\tvs stmt -> tvs <> ftv stmt) Set.empty stmts

    ftv e = Set.empty


instance Substitutable AST.Case where
    type Target AST.Case = Type
    apply sub (AST.TypedCase pat ty expr) = AST.TypedCase (apply sub pat) (apply sub ty) (apply sub expr)
    apply sub (AST.Case pat expr) = AST.Case (apply sub pat) (apply sub expr)

    ftv (AST.TypedCase pat ty expr) = ftv pat <> ftv ty <> ftv expr
    ftv (AST.Case pat expr)         = ftv pat <> ftv expr


instance Substitutable AST.Pattern where
    type Target AST.Pattern = Type
    apply sub (AST.PatTuple pats rest) = AST.PatTuple (apply sub <$> pats) rest
    apply sub (AST.PatList pats rest) = AST.PatList (apply sub <$> pats) rest
    apply sub (AST.PatRecord pairs rest) = AST.PatRecord (apply sub <$> pairs) rest
    apply sub (AST.PatData tag pats) = AST.PatData tag $ apply sub <$> pats

    apply sub pat = pat

    ftv (AST.PatTuple pats rest) = foldl (\tvs pat -> tvs <> ftv pat) Set.empty pats
    ftv (AST.PatList pats rest) = foldl (\tvs pat -> tvs <> ftv pat) Set.empty pats
    ftv (AST.PatRecord pairs rest) = foldl (\tvs pair -> tvs <> ftv pair) Set.empty pairs
    ftv (AST.PatData tag pats) = foldl (\tvs pat -> tvs <> ftv pat) Set.empty pats

    ftv pat = Set.empty


instance Substitutable AST.Statement where
    type Target AST.Statement = Type
    apply sub (AST.Return expr)     = AST.Return $ apply sub expr
    apply sub (AST.Procedure expr)  = AST.Procedure $ apply sub expr
    apply sub (AST.Declaration dec) = AST.Declaration $ apply sub dec

    ftv (AST.Return expr)     = ftv expr
    ftv (AST.Procedure expr)  = ftv expr
    ftv (AST.Declaration dec) = ftv dec


instance Substitutable AST.Declaration where
    type Target AST.Declaration = Type
    apply sub (AST.Let id tyExpr k expr) = AST.Let id tyExpr k $ apply sub expr
    apply sub d                          = d

    ftv (AST.Let id tyExpr k expr) = ftv expr
    ftv _                          = Set.empty
