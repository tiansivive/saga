{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Syntax.Desugared.AST where

import           Saga.Language.Syntax.AST

data instance AST Desugared e where
    Raw         :: Node Desugared e -> AST Desugared e
    Annotated   :: Node Desugared e -> Node Desugared (Annotation e) -> AST Desugared e

