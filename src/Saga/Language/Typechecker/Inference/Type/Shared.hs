
{-# LANGUAGE TypeFamilies #-}
module Saga.Language.Typechecker.Inference.Type.Shared where


import qualified Saga.Language.Core.Evaluated                  as TypeEvaluated
import qualified Saga.Language.Typechecker.Inference.Inference as I hiding
                                                                    (State,
                                                                     fresh)
import           Saga.Language.Typechecker.Inference.Inference hiding (State,
                                                                fresh)
import qualified Saga.Language.Typechecker.Kind                as K
import qualified Saga.Language.Typechecker.Solver.Constraints  as CST
import           Saga.Language.Typechecker.Type                (Scheme (..),
                                                                Type)

import qualified Effectful.State.Static.Local                  as Eff

import qualified Data.Map                                      as Map
import           Effectful                                     (Eff, (:>))
import qualified Effectful.Error.Static                        as Eff
import qualified Effectful.Reader.Static                       as Eff
import qualified Effectful.Writer.Static.Local                 as Eff
import           Saga.Language.Typechecker.Environment         (CompilerState (..))
import           Saga.Language.Typechecker.Errors              (SagaError (UnboundVariable))
import           Saga.Language.Typechecker.Monad               (TypeCheck)
import qualified Saga.Language.Typechecker.Qualification       as Q
import           Saga.Language.Typechecker.Qualification       (Given (..),
                                                                Qualified (..))
import qualified Saga.Language.Typechecker.Type                as T
import qualified Saga.Language.Typechecker.Variables           as Var
import           Saga.Language.Typechecker.Variables
import           Saga.Utils.Operators                          ((|>))
import           Saga.Utils.TypeLevel                          (type (§))



type TypeInference es = (TypeCheck es, Eff.Reader Var.Level :> es, Eff.State State :> es, Eff.Writer CST.Constraint :> es)


data State = IST
  { tvars  :: Int
  , evars  :: Int
  , levels :: Map.Map (Variable Type) Var.Level
  , proofs :: Map.Map Type Type
  } deriving (Show)


initialState :: State
initialState = IST 0 0 Map.empty Map.empty

type Constructor = String -> K.Kind -> Variable Type
fresh :: (Eff.Reader Var.Level :> es, Eff.State State :> es) => Constructor -> Eff es § Variable Type
fresh constructor = do
  i <- Eff.gets $ tvars |> (+1)
  Eff.modify $ \s -> s { tvars = i }
  let count = show ([1 ..] !! i)
  let tvar = constructor ("t" ++ count) K.Type
  lvl <- Eff.ask @Var.Level
  Eff.modify $ \s -> s { levels = Map.insert tvar lvl $ levels s }
  return tvar


lookup :: (TypeInference es, Instantiate Type) => String -> Eff es (Qualified Type)
lookup x = do
  Saga { types } <- Eff.ask

  qt@(bs :| cs :=> t) <- case Map.lookup x types of
    Just scheme -> walk scheme
    Nothing     -> Eff.throwError $ UnboundVariable x

  proofs' <- Eff.gets proofs
  case Map.lookup t proofs' of
    Just t' -> return $ bs :| cs :=> t'
    Nothing -> return qt

  where
    walk scheme@(Forall [] qt) = return qt
    walk scheme@(Forall tvars _) = fresh T.Unification >>= walk . instantiate scheme . T.Var



mkEvidence :: (Eff.State State :> es) => Eff es § Variable CST.Evidence
mkEvidence = do
  i <- Eff.gets $ evars |> (+1)
  Eff.modify $ \s -> s { evars = i }
  let count = show ([1 ..] !! i)
  return $ CST.Evidence ("ev_" ++ count)




toItem :: Type -> CST.Item
toItem (T.Var tvar) = CST.Var tvar
toItem t            = CST.Mono t


propagate :: TypeInference es => T.Constraint -> Eff es ()
propagate (ty `Q.Implements` prtcl) = mkEvidence >>= \e -> Eff.tell $ CST.Impl e (CST.Mono ty) prtcl
propagate (Q.Resource mul ty) = Eff.tell $ CST.Resource (CST.Mono ty) mul
propagate (Q.Pure ty) = Eff.tell $ CST.Pure (CST.Mono ty)
propagate (Q.Refinement bs expr ty) = Eff.tell $ CST.Refined (fmap CST.Mono bs) (CST.Mono ty) expr





