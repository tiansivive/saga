{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Core.AST where





data family AST (p :: Phase) (e :: NodeType)
data family Node (p :: Phase) (t:: NodeType)
data NodeType = Expression | Declaration | Statement | Case | Pattern | Type | Kind
    deriving (Show, Eq, Ord)
data Phase = Parsed | Desugared | Evaluated | Elaborated | Zonked | TypeChecked
    deriving (Show, Eq, Ord)
type family Annotation (n :: NodeType) where
    Annotation Expression   = Type
    Annotation Declaration  = Type
    Annotation Statement    = Type
    Annotation Case         = Type
    Annotation Pattern      = Type
    Annotation Type         = Kind
    Annotation Kind         = Kind



