{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Syntax.Evaluated.AST where

import           Saga.Language.Syntax.AST

import           Saga.Utils.TypeLevel     (type (§))

data instance AST Evaluated e where
    Raw         :: Node Evaluated e -> AST Evaluated e
    Annotated   :: Node Evaluated e -> Node Evaluated (Annotation e) -> AST Evaluated e

