{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Syntax.Zonked.AST where

import           Saga.Language.Syntax.AST

data instance AST Zonked e where
    -- FIXME:QUESTION: Remove this to guarantee all AST Nodes are annotated?
    Raw         :: Node Zonked e -> AST Zonked e
    Annotated   :: Node Zonked e -> Node Zonked (Annotation e) -> AST Zonked e

