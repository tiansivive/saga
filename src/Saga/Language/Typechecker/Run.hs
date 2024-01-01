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
import           Saga.Language.Typechecker.Errors                        (SagaError)
import qualified Saga.Language.Typechecker.Inference.Inference           as I
import qualified Saga.Language.Typechecker.Inference.Type.Expr           as TI
import qualified Saga.Language.Typechecker.Inference.Type.Shared         as TI

import           Saga.Language.Typechecker.Evaluation                    (Evaluate (evaluate))
import           Saga.Language.Typechecker.Inference.Type.Shared         (State (levels))
import           Saga.Language.Typechecker.Lib                           (defaultEnv)
import           Saga.Language.Typechecker.Monad                         (TypeCheck,
                                                                          run)
import qualified Saga.Language.Typechecker.Solver.Constraints            as CST
import           Saga.Language.Typechecker.Solver.Cycles                 (Cycle)
import           Saga.Language.Typechecker.Solver.Monad                  (Count (..),
                                                                          Solve (..),
                                                                          initialCount,
                                                                          initialSolution)
import qualified Saga.Language.Typechecker.Solver.Run                    as Solver
import qualified Saga.Language.Typechecker.Solver.Shared                 as Shared
import           Saga.Language.Typechecker.Type                          (Polymorphic,
                                                                          Type)
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
        ((ast, st), constraint) <- infer' expr
        pTraceM $ "\nAST:\n" ++ show ast
        context <- Eff.inject $ Solver.run (constraint, levels st)
        (ast', ty) <- Zonking.run ast context

        return (ast', ty)

instance Typecheck Script where
    type Out Script = Polymorphic Type
    typecheck (Script [decs]) = do
        return _f

instance Typecheck Declaration where

    typecheck :: TypeCheck es => Declaration -> Eff es (Declaration, Out Declaration)
    typecheck (Let id tyExpr k expr) = do
        ty' <- mapM evaluate tyExpr
        ((ast, st), constraint) <- infer' expr


        _f



infer' :: (Eff.Reader CompilerState Eff.:> es, Eff.Writer Info Eff.:> es, Eff.Error SagaError Eff.:> es, Eff.IOE Eff.:> es, Eff.Fail Eff.:> es) => Expr -> Eff es ((Expr, State), CST.Constraint)
infer' = Eff.runWriter @CST.Constraint . Eff.runState TI.initialState . Eff.runReader (Var.Level 0) . infer


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


test e = run $ do
    ((e, st), cs) <- TI.run e
    Eff.runState initialSolution . Eff.evalState initialCount .  Eff.runState @[Cycle Type] [] . Eff.runReader (Var.Level 0) . Eff.runReader (levels st) $ simplified cs
        where
            simplified = mapM simplify . Shared.flatten



