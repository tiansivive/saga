{-# LANGUAGE ExplicitNamespaces #-}

module Saga.Language.Typechecker.Elaboration.Values.Shared where



import           Effectful                                            (Eff,
                                                                       (:>))
import qualified Effectful.Reader.Static                              as Eff
import qualified Effectful.State.Static.Local                         as Eff
import qualified Effectful.Writer.Static.Local                        as Eff
import           Saga.Language.Typechecker.Elaboration.Values.Effects (State (..))
import qualified Saga.Language.Typechecker.Variables                  as Var

import qualified Data.Map                                             as Map

import qualified Saga.Language.Syntax.Elaborated.Kinds                as K
import qualified Saga.Language.Syntax.Elaborated.Types                as T
import           Saga.Language.Syntax.Elaborated.Types                (Type)
import qualified Saga.Language.Typechecker.Solver.Constraints         as CST
import           Saga.Language.Typechecker.Variables                  (Variable)
import           Saga.Utils.Operators                                 ((|>))
import           Saga.Utils.TypeLevel                                 (type (§))


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

mkEvidence :: (Eff.State State :> es) => Eff es § Variable CST.Evidence
mkEvidence = do
  i <- Eff.gets $ evars |> (+1)
  Eff.modify $ \s -> s { evars = i }
  let count = show ([1 ..] !! i)
  return $ CST.Evidence ("ev_" ++ count)




toItem :: Type -> CST.Item
toItem (T.Var tvar) = CST.Var _tvar
toItem t            = CST.Mono _t




