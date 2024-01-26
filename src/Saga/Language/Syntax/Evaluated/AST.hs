{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Syntax.Evaluated.AST where

import           Saga.Language.Syntax.AST

import           Saga.Utils.TypeLevel     (type (§))

data instance AST Evaluated e where
    Raw         :: Node Evaluated e -> AST Evaluated e
    Annotated   :: Node Evaluated e -> AST Evaluated (Annotation e) -> AST Evaluated e

node :: AST 'Evaluated e -> Node 'Evaluated e
node (Annotated node _) = node
node (Raw node)         = node

annotation :: Show (Node 'Evaluated e) => AST 'Evaluated e -> AST 'Evaluated (Annotation e)
annotation (Annotated _ ann) = ann
annotation (Raw node)        = error $ "Tried to extract annotation from raw node: " ++ show node
