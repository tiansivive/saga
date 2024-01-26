{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Typechecker.Elaboration.Types where

import qualified Saga.Language.Syntax.AST                             as NT (NodeType (..))
import           Saga.Language.Syntax.Evaluated.Types                 (Type)

import           Saga.Language.Typechecker.Elaboration.Monad          (Effects,
                                                                       Elaboration (..))
import qualified Saga.Language.Typechecker.Elaboration.Values.Effects as Effs



-- TODO: This is kind inference via elaboration of types.
instance Elaboration NT.Type where
    type Effects NT.Type es = Effs.Elaboration es
    elaborate = undefined
