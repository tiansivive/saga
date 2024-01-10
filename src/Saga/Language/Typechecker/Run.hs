{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeFamilies       #-}
module Saga.Language.Typechecker.Run where
import qualified Saga.Language.Core.Expr                                 as E
import           Saga.Language.Core.Expr                                 (Declaration (..),
                                                                          Expr,
                                                                          Script (..))
import           Saga.Language.Typechecker.Inference.Inference           (Inference (Effects, infer))

import           Saga.Language.Typechecker.Inference.Type.Generalization
import           Saga.Language.Typechecker.Inference.Type.Instantiation

import           Debug.Pretty.Simple                                     (pTrace,
                                                                          pTraceM)
import           Effectful                                               (Eff)
import qualified Effectful                                               as Eff
import qualified Effectful.Error.Static                                  as Eff
import qualified Effectful.Fail                                          as Eff
import qualified Effectful.Reader.Static                                 as Eff
import qualified Effectful.State.Static.Local                            as Eff
import qualified Effectful.Writer.Static.Local                           as Eff
import           Saga.Language.Core.Literals                             (Literal (..))
import           Saga.Language.Typechecker.Environment                   (CompilerState,
                                                                          Info)
import           Saga.Language.Typechecker.Errors                        (SagaError (PolymorphicToConcreteMismatch))
import qualified Saga.Language.Typechecker.Inference.Inference           as I
import qualified Saga.Language.Typechecker.Inference.Type.Expr           as TI
import qualified Saga.Language.Typechecker.Inference.Type.Shared         as TI

import           Control.Monad                                           (when)
import           Data.Maybe                                              (fromMaybe)
import           Saga.Language.Typechecker.Evaluation                    (Evaluate (evaluate))
import           Saga.Language.Typechecker.Inference.Type.Shared         (State (levels))
import qualified Saga.Language.Typechecker.Kind                          as K
import           Saga.Language.Typechecker.Lib                           (defaultEnv)
import           Saga.Language.Typechecker.Monad                         (TypeCheck,
                                                                          run)
import qualified Saga.Language.Typechecker.Qualification                 as Q
import           Saga.Language.Typechecker.Qualification                 (Qualified (..))
import qualified Saga.Language.Typechecker.Solver.Constraints            as CST
import           Saga.Language.Typechecker.Solver.Cycles                 (Cycle)
import           Saga.Language.Typechecker.Solver.Monad                  (Count (..),
                                                                          Solve (..),
                                                                          initialCount,
                                                                          initialSolution)
import qualified Saga.Language.Typechecker.Solver.Run                    as Solver
import qualified Saga.Language.Typechecker.Solver.Shared                 as Shared
import qualified Saga.Language.Typechecker.Type                          as T
import           Saga.Language.Typechecker.Type                          (Polymorphic,
                                                                          Type)
import qualified Saga.Language.Typechecker.TypeExpr                      as TE
import qualified Saga.Language.Typechecker.Variables                     as Var
import qualified Saga.Language.Typechecker.Zonking.Run                   as Zonking
import           Saga.Utils.Operators                                    ((|>))
import           Saga.Utils.TypeLevel                                    (type (§))


class Typecheck e where
    type Out e
    typecheck :: TypeCheck es => e -> Eff es (e, Out e)


instance Typecheck Expr where
    type Out Expr = Polymorphic Type
    typecheck expr  = do
        ((ast, st), constraint) <- TI.run expr
        pTraceM $ "\nAST:\n" ++ show ast

        context <- Eff.inject $ Solver.run (constraint, levels st)
        pTraceM $ "\nCONTEXT::\n" ++ show context
        (ast', ty) <- Zonking.run ast context

        return (ast', ty)

-- instance Typecheck Script where
--     type Out Script = Polymorphic Type
--     typecheck (Script [decs]) = do
--         return _f

instance Typecheck Declaration where

    --typecheck :: TypeCheck es => Declaration -> Eff es (Declaration, Out Declaration)
    type Out Declaration = Polymorphic Type
    typecheck d@(Let id tyExpr k expr) = do
        ty <- mapM evaluate tyExpr
        (ast, ty'@(T.Forall tvars' qt')) <- typecheck expr

        case ty of
            Nothing         -> return (Let id tyExpr k ast, ty')
            Just annotated@(T.Forall tvars qt)  -> do
                -- TODO:FIXME: This is incomplete.
                -- We need to check that the annotated type is a subtype of the inferred type.
                -- This should be done by rigifying the annotated type and then unifying with the inferred type
                -- Protocol implementations should be checked as well.
                -- SUGGESTION: Perhaps the best way is by copying the `Stmt` inference code and using the same machinery.
                when (length tvars > length tvars') (Eff.throwError $ PolymorphicToConcreteMismatch annotated ty')
                return (Let id tyExpr k ast, annotated)




int = E.Literal . LInt

str = E.Literal . LString


gt x y = E.FnApp (E.Identifier ">") [x, y]
gte x y = E.FnApp (E.Identifier ">=") [x, y]
lt x y = E.FnApp (E.Identifier "<") [x, y]
lte x y = E.FnApp (E.Identifier "<=") [x, y]


op x = E.FnApp (E.Identifier "/") [E.Literal $ LInt 1, x]

cases = E.Lambda ["x"] $
        E.Match (E.Identifier "x")
            [ E.Case (E.Lit $ LInt 1) (E.Identifier "x")
            , E.Case (E.Lit $ LString "Hello") (E.Literal $ LString "World")
            ]

hofType = T.Forall [T.Poly "a" K.Type] $ Q.none :=> (T.Data "Int" K.Type `T.Arrow` T.Data "Int" K.Type) `T.Arrow` T.Data "Int" K.Type
hofTypeExpr = (TE.Identifier "Int" `TE.Arrow` TE.Identifier "Int") `TE.Arrow` TE.Identifier "Int"
hof = E.Lambda ["f"] $ E.FnApp (E.Identifier "f") [E.Literal $ LInt 1]

dec = Let "hof" (Just hofTypeExpr) Nothing hof


tc d = run $ typecheck d

test e = run $ do
    ((e, st), cs) <- TI.run e
    Eff.runState initialSolution . Eff.evalState initialCount .  Eff.runState @[Cycle Type] [] . Eff.runReader (Var.Level 0) . Eff.runReader (levels st) $ simplified cs
        where
            simplified = mapM simplify . Shared.flatten


