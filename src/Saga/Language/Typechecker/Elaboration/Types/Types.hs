{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Typechecker.Elaboration.Types.Types where

import qualified Saga.Language.Syntax.AST                             as NT (NodeType (..))
import           Saga.Language.Syntax.Evaluated.Types                 (Type)

import qualified Saga.Language.Syntax.Elaborated.AST                  as EL
import qualified Saga.Language.Syntax.Elaborated.Kinds                as EK
import qualified Saga.Language.Syntax.Elaborated.Types                as ET
import qualified Saga.Language.Syntax.Evaluated.AST                   as AST
import qualified Saga.Language.Syntax.Evaluated.Types                 as EV
import           Saga.Language.Typechecker.Elaboration.Monad          (Effects,
                                                                       Elaboration (..))
import qualified Saga.Language.Typechecker.Elaboration.Types.Shared   as Shared
import qualified Saga.Language.Typechecker.Elaboration.Values.Effects as Effs



-- TODO: This is kind inference via elaboration of types.
instance Elaboration NT.Type where
    type Effects NT.Type es = Effs.Elaboration es

    -- elaborate (AST.Raw node) = case node of
    --     EV.Var name -> do
    --         kvar <- EK.Var <$> Shared.fresh
    --         ty <- Shared.lookup name
    --         implication@(CST.Implication _ assumptions _) <- Shared.contextualize $ EL.node ty
    --         Eff.tell implication

    --         let e' = foldl elaborate' (EL.Var $ EL.Identifier name) assumptions
    --         return $ EL.Annotated e' ty

    --         where
    --             elaborate' expr (CST.Assume c)
    --                 -- | QUESTION: Do we need to also annotate types here or de we expand it during Zonking?
    --                 | CST.Impl (CST.Evidence e) _ protocol <- c = EL.Application (EL.Raw expr) [EL.Raw (EL.Var $ EL.Evidence e)]
    --                 | otherwise                                 = expr

            --return (EL.Annotated (ET.Var v) (EL.Raw kvar))
    elaborate t         = undefined
