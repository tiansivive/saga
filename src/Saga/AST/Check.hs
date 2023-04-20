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
    case Infer.reduce ty of
        ty'@(TVar id) -> ty' `isSubtype` inferred
        ty'           -> inferred `isSubtype` ty'


