{-# LANGUAGE TypeFamilies #-}
module Saga.Language.Typechecker.Inference.Type.Statements where
import qualified Data.Map                                        as Map
import           Effectful                                       (Eff)
import qualified Effectful.Reader.Static                         as Eff
import qualified Effectful.Writer.Static.Local                   as Eff
import qualified Saga.Language.Core.Evaluated                    as TypeEvaluated
import           Saga.Language.Typechecker.Inference.Inference   (Inference (..))
import qualified Saga.Language.Typechecker.Inference.Type.Shared as Shared
import           Saga.Language.Typechecker.Monad                 (TypeCheck)
import qualified Saga.Language.Typechecker.Qualification         as Q
import           Saga.Language.Typechecker.Qualification         (Qualified ((:=>)))
import qualified Saga.Language.Typechecker.Solver.Constraints    as C
import qualified Saga.Language.Typechecker.Type                  as T

import           Saga.Language.Typechecker.Environment           (CompilerState (..))


instance Inference Declaration where

    type instance Effects Declaration es = Shared.TypeInference es

    infer = infer'
    -- lookup = lookup'
    -- fresh = Shared.fresh T.Unification


infer' :: TypeCheck es => Declaration -> Eff es (Declaration, T.Type)
infer' (Let id Nothing k expr) = do
    tvar <- Shared.fresh T.Unification
    let qt = T.Forall [] (Q.none :=> T.Var tvar)
    let scoped = Eff.local (\e -> e{ types = Map.insert id qt $ types e })
    scoped $ do
      (Typed expr' ty) <- infer expr
      ev <- Shared.mkEvidence
      Eff.tell $ C.Equality ev (C.Var tvar) (C.Mono ty)
      return (Let id (Just ty) k expr', ty)

