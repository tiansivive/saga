{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE TypeData               #-}
{-# LANGUAGE TypeFamilyDependencies #-}


module Saga.Language.Syntax.AST where
import           Data.Data    (Typeable)
import qualified Data.Kind    as GHC
import           GHC.TypeLits (ErrorMessage (Text), TypeError)



data family AST (p :: Phase) (e :: NodeType)
data family Node (p :: Phase) (t:: NodeType)
type data NodeType
    = Expression | Declaration | Statement
    | Type | Kind
    | Case NodeType | Pattern NodeType
    | Constraint | Liquid

type data Phase = Parsed | Reduced | Elaborated | TypeChecked


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


