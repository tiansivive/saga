
{-# LANGUAGE TypeFamilies #-}
module Saga.Language.Typechecker.Inference.Type.Shared where


import           Saga.Language.Core.Expr                       (Expr)
import qualified Saga.Language.Typechecker.Inference.Inference as I hiding
                                                                    (fresh)
import           Saga.Language.Typechecker.Inference.Inference hiding (fresh)
import qualified Saga.Language.Typechecker.Kind                as K
import qualified Saga.Language.Typechecker.Solver.Constraints  as CST
import           Saga.Language.Typechecker.Type                (Type)

import qualified Effectful.State.Static.Local                  as Eff

import           Effectful                                     (Eff)
import qualified Effectful.Writer.Static.Local                 as Eff
import qualified Saga.Language.Typechecker.Qualification       as Q
import qualified Saga.Language.Typechecker.Type                as T
import qualified Saga.Language.Typechecker.Variables           as Var
import           Saga.Language.Typechecker.Variables



type TypeInference es = InferEff es CST.Constraint

type instance I.EmittedConstraint Type = CST.Constraint

type instance VarType Expr I.Evidence       = Var.Variable CST.Evidence
type instance VarType Expr I.Unification    = Var.Variable Type
type instance VarType Expr I.Skolem         = Var.Variable Type
type instance VarType Expr I.TypeVar        = Var.Variable Type
type instance VarType Expr I.Instantiation  = Var.Variable Type




fresh :: TypeInference es => Tag a -> Eff es (VarType Expr a)
fresh t = do
  Eff.modify $ \s -> s {vars = vars s + 1}
  s <- Eff.get
  let count = show ([1 ..] !! vars s)
  return $ case t of
    E -> CST.Evidence $ "e" ++ count
    U -> T.Unification ("v" ++ count) K.Type
    T -> T.Poly ("p" ++ count) K.Type


propagate :: TypeInference es => T.Constraint -> Eff es ()
propagate (ty `Q.Implements` prtcl) = fresh E >>= \e -> Eff.tell $ CST.Impl e (CST.Mono ty) prtcl
propagate (Q.Resource mul ty) = Eff.tell $ CST.Resource (CST.Mono ty) mul
propagate (Q.Pure ty) = Eff.tell $ CST.Pure (CST.Mono ty)
propagate (Q.Refinement bs expr ty) = Eff.tell $ CST.Refined (fmap CST.Mono bs) (CST.Mono ty) expr
