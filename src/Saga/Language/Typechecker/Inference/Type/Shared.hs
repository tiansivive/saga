
{-# LANGUAGE TypeFamilies #-}
module Saga.Language.Typechecker.Inference.Type.Shared where


import           Saga.Language.Core.Expr                       (Expr)
import qualified Saga.Language.Typechecker.Inference.Inference as I hiding
                                                                    (State,
                                                                     fresh)
import           Saga.Language.Typechecker.Inference.Inference hiding (State,
                                                                fresh)
import qualified Saga.Language.Typechecker.Kind                as K
import qualified Saga.Language.Typechecker.Solver.Constraints  as CST
import           Saga.Language.Typechecker.Type                (Type)

import qualified Effectful.State.Static.Local                  as Eff

import           Effectful                                     (Eff, (:>))
import qualified Effectful.Reader.Static                       as Eff
import qualified Effectful.Writer.Static.Local                 as Eff
import           Saga.Language.Typechecker.Monad               (TypeCheck)
import qualified Saga.Language.Typechecker.Qualification       as Q
import qualified Saga.Language.Typechecker.Type                as T
import qualified Saga.Language.Typechecker.Variables           as Var
import           Saga.Language.Typechecker.Variables
import           Saga.Utils.Operators                          ((|>))
import           Saga.Utils.TypeLevel                          (type (§))



type TypeInference es = (TypeCheck es, Eff.Reader CST.Level :> es, Eff.State State :> es, Eff.Writer CST.Constraint :> es)


data State = IST
  { tvars :: Int
  , evars :: Int
  } deriving (Show)


initialState :: State
initialState = IST 0 0


fresh :: (Eff.State State :> es) => Eff es § Variable Type
fresh = do
  i <- Eff.gets $ tvars |> (+1)
  Eff.modify $ \s -> s { tvars = i }
  let count = show ([1 ..] !! i)
  return $ T.Poly ("t" ++ count) K.Type

mkEvidence :: (Eff.State State :> es) => Eff es § Variable CST.Evidence
mkEvidence = do
  i <- Eff.gets $ evars |> (+1)
  Eff.modify $ \s -> s { evars = i }
  let count = show ([1 ..] !! i)
  return $ CST.Evidence ("ev_" ++ count)




toItem :: (Variable Type -> Variable CST.Item) -> Type -> CST.Item
toItem constructor (T.Var tvar) = CST.Variable (constructor tvar)
toItem _ t                      = CST.Mono t


propagate :: TypeInference es => T.Constraint -> Eff es ()
propagate (ty `Q.Implements` prtcl) = mkEvidence >>= \e -> Eff.tell $ CST.Impl e (CST.Mono ty) prtcl
propagate (Q.Resource mul ty) = Eff.tell $ CST.Resource (CST.Mono ty) mul
propagate (Q.Pure ty) = Eff.tell $ CST.Pure (CST.Mono ty)
propagate (Q.Refinement bs expr ty) = Eff.tell $ CST.Refined (fmap CST.Mono bs) (CST.Mono ty) expr





