{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}

module Saga.Language.Syntax.AST where
import qualified Data.Kind    as GHC
import           GHC.TypeLits (ErrorMessage (Text), TypeError)



data family AST (p :: Phase) (e :: NodeType)
data family Node (p :: Phase) (t:: NodeType)
data NodeType
    = Expression | Declaration | Statement
    | Type | Kind
    | Case NodeType | Pattern NodeType
    | Constraint | Liquid
    deriving (Show, Eq, Ord)
data Phase = Parsed | Desugared | Evaluated | Elaborated | Zonked | TypeChecked
    deriving (Show, Eq, Ord)
type family Annotation (n :: NodeType) where
    Annotation Expression           = Type
    Annotation Declaration          = TypeError ('Text "Declarations must not have an Annotation")
    Annotation Statement            = Type
    Annotation Type                 = Kind
    Annotation Kind                 = Kind
    Annotation (Case Expression)    = Type
    Annotation (Case Type)          = Kind
    Annotation (Case Kind)          = Kind
    Annotation (Pattern Expression) = Type
    Annotation (Pattern Type)       = Kind
    Annotation (Pattern Kind)       = Kind
    Annotation (Case _)             = TypeError ('Text "Case annotation must be applied to Expression, Type or Kind")
    Annotation (Pattern _)          = TypeError ('Text "Pattern annotation must be applied to Expression, Type or Kind")
    Annotation Constraint           = Kind
    Annotation Liquid               = Type



class Monad m => Visitor m (node :: NodeType) where
  type Pass node :: Phase
  visit :: (Node (Pass node) node -> m (Node (Pass node) node)) -> Node (Pass node) node -> m (Node (Pass node) node)
