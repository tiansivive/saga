{-# LANGUAGE GADTs #-}

module Saga.Language.Typechecker.Solver.Constraints where
import           Saga.Language.Typechecker.Kind          (Kind)
import           Saga.Language.Typechecker.Protocols     (Implementation,
                                                          ProtocolID)
import qualified Saga.Language.Core.Liquid as Liquid
import           Saga.Language.Typechecker.Type          (Polymorphic, Type)
import           Saga.Language.Typechecker.Variables


data Constraint where
    Empty       :: Constraint
    Conjunction :: Constraint -> Constraint -> Constraint
    Equality    :: Evidence -> Item -> Item -> Constraint
    Impl        :: Evidence -> Item -> ProtocolID -> Constraint
    OneOf       :: Item -> Item -> Constraint
    Refine      :: Item -> Liquid.Expr -> Constraint
    Consumed    :: Int -> Item -> Constraint
    Impure      :: Item -> Constraint
    Implication :: [Assumption] -> [Constraint] -> Constraint


data Item
    = Skolem String Kind
    | Unification Level (PolymorphicVar Type)
    | Mono Type
    | Poly (Polymorphic Type)


data Assumption
    = ForType Evidence Item
    | ForValue Item Liquid.Expr

newtype ID = ID String


data Evidence
    = Var String
    | Protocol Implementation
    | Coercion Mechanism

data Mechanism = Nominal | Structural
