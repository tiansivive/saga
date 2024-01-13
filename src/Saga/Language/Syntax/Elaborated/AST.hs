{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Syntax.Elaborated.AST where

import           Saga.Language.Syntax.AST

data instance AST Elaborated e where
    -- FIXME:QUESTION: Remove this to guarantee all AST Nodes are annotated?
    Raw         :: Node Elaborated e -> AST Elaborated e
    Annotated   :: Node Elaborated e -> Node Elaborated (Annotation e) -> AST Elaborated e

