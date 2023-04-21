module Saga.AST.TypeSystem.Check where


import qualified Saga.AST.TypeSystem.Inference       as Infer
import           Saga.AST.TypeSystem.Inference       (Infer)
import           Saga.AST.TypeSystem.Subtyping
import           Saga.AST.TypeSystem.Types
import           Saga.AST.Syntax

import           Control.Monad.State.Lazy
import qualified Data.Map                 as Map






check :: (Eq a, Show a) => Expr a -> TypeExpr a -> Infer a Bool
check expr ty = do
    inferred <- Infer.typeof expr
    case Infer.reduce ty of
        ty'@(TVar id) -> ty' `isSubtype` inferred
        ty'           -> inferred `isSubtype` ty'


