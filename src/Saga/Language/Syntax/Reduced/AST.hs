{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Syntax.Reduced.AST where

import           Saga.Language.Syntax.AST

import           Saga.Utils.TypeLevel     (type (§))

data instance AST Reduced e where
    Raw         :: Node Reduced e -> AST Reduced e
    Annotated   :: Node Reduced e -> AST Reduced (Annotation e) -> AST Reduced e

node :: AST Reduced e -> Node Reduced e
node (Annotated node _) = node
node (Raw node)         = node

annotation :: Show (Node Reduced e) => AST Reduced e -> AST Reduced (Annotation e)
annotation (Annotated _ ann) = ann
annotation (Raw node)        = error $ "Tried to extract annotation from raw node: " ++ show node
