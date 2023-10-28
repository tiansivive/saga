
{-# LANGUAGE TypeFamilies #-}
module Saga.Language.Typechecker.Inference.Type.Shared where

import           Control.Monad.RWS
import           Saga.Language.Core.Expr                       (Expr)
import qualified Saga.Language.Typechecker.Inference.Inference as I hiding
                                                                    (fresh)
import           Saga.Language.Typechecker.Inference.Inference hiding (fresh)
import qualified Saga.Language.Typechecker.Kind                as K
import qualified Saga.Language.Typechecker.Solver.Constraints  as CST
import           Saga.Language.Typechecker.Type                (Type)

import qualified Saga.Language.Typechecker.Qualification       as Q
import qualified Saga.Language.Typechecker.Type                as T
import qualified Saga.Language.Typechecker.Variables           as Var
import           Saga.Language.Typechecker.Variables



type TypeInference m = InferM CST.Constraint m

type instance I.EmittedConstraint Type = CST.Constraint

type instance VarType Expr I.Evidence       = Var.PolymorphicVar CST.Evidence
type instance VarType Expr I.Unification    = Var.PolymorphicVar Type
type instance VarType Expr I.Skolem         = Var.PolymorphicVar Type
type instance VarType Expr I.PolyVar        = Var.PolymorphicVar Type
type instance VarType Expr I.Instantiation  = Var.PolymorphicVar Type




fresh :: TypeInference m => Tag a -> m (VarType Expr a)
fresh t = do
  modify $ \s -> s {vars = vars s + 1}
  s <- get
  let count = show ([1 ..] !! vars s)
  return $ case t of
    E -> Var.Evidence $ "e" ++ count
    U -> Var.Unification ("v" ++ count) (Level $ level s) K.Type
    P -> Var.PolyVar ("p" ++ count) K.Type


propagate :: TypeInference m => T.Constraint -> m ()
propagate (ty `Q.Implements` prtcl) = fresh E >>= \e -> I.emit' $ CST.Impl e (CST.Mono ty) prtcl
propagate (Q.Resource mul ty) = I.emit' $ CST.Resource (CST.Mono ty) mul
propagate (Q.Pure ty) = I.emit' $ CST.Pure (CST.Mono ty)
propagate (Q.Refinement expr ty) = I.emit' $ CST.Refined (CST.Mono ty) expr
