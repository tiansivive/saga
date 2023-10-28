{-# LANGUAGE GADTs #-}

module Saga.Language.Typechecker.Solver.Constraints where
import qualified Saga.Language.Core.Liquid               as Liquid
import           Saga.Language.Typechecker.Kind          (Kind)
import           Saga.Language.Typechecker.Protocols     (Implementation,
                                                          ProtocolID)
import           Saga.Language.Typechecker.Qualification (Multiplicity)
import           Saga.Language.Typechecker.Type          (Polymorphic, Type)
import           Saga.Language.Typechecker.Variables


import qualified Saga.Language.Typechecker.Type          as T


data Constraint where
    Empty       :: Constraint
    Conjunction :: Constraint -> Constraint -> Constraint
    Equality    :: Evidence -> Item -> Item -> Constraint
    Impl        :: Evidence -> Item -> ProtocolID -> Constraint
    OneOf       :: Item -> Item -> Constraint
    Refined     :: Item -> Liquid.Expr -> Constraint
    Resource    :: Item -> Multiplicity -> Constraint
    Consumed    :: Item -> Constraint
    Pure        :: Item -> Constraint
    Impure      :: Item -> Constraint
    Implication :: [PolymorphicVar Type] -> [Assumption] -> Constraint -> Constraint

deriving instance Show Constraint

data Item
    = Skolem String Kind
    | Unification Level (PolymorphicVar Type)
    | Mono Type
    | Poly (Polymorphic Type)
    deriving (Show)

data Assumption
    = TypeLevel  T.Constraint
    | ValueLevel T.Constraint
    | Binding String (PolymorphicVar Type)
    deriving (Show)

data Evidence
    = Var String
    | Protocol Implementation
    | Coercion Mechanism
    deriving (Show)

data Mechanism = Nominal | Structural deriving (Show)
