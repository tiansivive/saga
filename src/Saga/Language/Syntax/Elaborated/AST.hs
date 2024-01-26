{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Saga.Language.Syntax.Elaborated.AST where

import           Saga.Language.Syntax.AST

data instance AST Elaborated e where
    -- FIXME:QUESTION: Remove this to guarantee all AST Nodes are annotated?
    Raw         :: Node Elaborated e -> AST Elaborated e
    Annotated   :: Node Elaborated e -> AST Elaborated (Annotation e) -> AST Elaborated e

node :: AST 'Elaborated e -> Node 'Elaborated e
node (Annotated node _) = node
node (Raw node)         = node

annotation :: Show (Node 'Elaborated e) => AST 'Elaborated e -> AST 'Elaborated (Annotation e)
annotation (Annotated _ ann) = ann
annotation (Raw node)        = error $ "Tried to extract annotation from raw node: " ++ show node
