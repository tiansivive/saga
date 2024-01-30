{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Typechecker.Elaboration.Types.Types where

import           Control.Monad                                        (forM)
import qualified Effectful.State.Static.Local                         as Eff
import qualified Saga.Language.Syntax.AST                             as NT (NodeType (..))
import qualified Saga.Language.Syntax.Elaborated.AST                  as EL
import qualified Saga.Language.Syntax.Elaborated.Kinds                as EK
import qualified Saga.Language.Syntax.Elaborated.Types                as ET
import qualified Saga.Language.Syntax.Evaluated.AST                   as AST
import qualified Saga.Language.Syntax.Evaluated.Types                 as EV
import           Saga.Language.Syntax.Evaluated.Types                 (Type)
import qualified Saga.Language.Typechecker.Elaboration.Effects        as Effs
import           Saga.Language.Typechecker.Elaboration.Effects        (State (..))
import           Saga.Language.Typechecker.Elaboration.Monad          (Effects,
                                                                       Elaboration (..),
                                                                       Generalize (..))
import qualified Saga.Language.Typechecker.Elaboration.Types.Shared   as Shared
import           Saga.Utils.Common                                    (forM2)

import           Saga.Language.Typechecker.Elaboration.Generalization

-- TODO: This is kind inference via elaboration of types.
instance Elaboration NT.Type where
  type Effects NT.Type es = Effs.Elaboration es

  elaborate (AST.Raw node) = case node of
    EV.Data name -> do
      k <- Shared.lookup name
      return $ EL.Annotated (ET.Data name) k

    EV.Singleton lit -> return $ EL.Annotated (ET.Singleton lit) (EL.Raw EK.Type)
    EV.Tuple elems -> do
      elems' <- forM elems elaborate
      return $ EL.Annotated (ET.Tuple elems') (EL.Raw EK.Type)
    EV.Record pairs ->  do
      pairs' <- forM2 pairs elaborate
      return $ EL.Annotated (ET.Record pairs') (EL.Raw EK.Type)
    EV.Union elems -> do
      elems' <- forM elems elaborate
      return $ EL.Annotated (ET.Union elems') (EL.Raw EK.Type)
    EV.Arrow in' out -> do
      in'' <- elaborate $ AST.Raw in'
      out' <- elaborate $ AST.Raw out
      return $ EL.Annotated (ET.Arrow in'' out') (EL.Raw EK.Type)

    EV.Applied cons arg -> do
      out <- EK.Var <$> Shared.fresh
      cons' <- elaborate cons
      arg' <- elaborate arg

      inferred <- generalize' $ EL.annotation arg' `EK.Arrow` EL.Raw out
      return $ EL.Annotated (ET.Applied cons' arg') (EL.Raw EK.Type)
      where
        generalize' ty = do
          count <- Eff.gets kvars
          (inferred, count') <- Eff.runState count $ generalize ty
          Eff.modify $ \s -> s { kvars = count' }
          return inferred

    EV.Var name -> return $ EL.Annotated (ET.Var name) k

  elaborate t = undefined