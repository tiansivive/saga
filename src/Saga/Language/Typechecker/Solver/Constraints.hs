{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Typechecker.Solver.Constraints where
import qualified Saga.Language.Core.Liquid                     as Liquid
import           Saga.Language.Typechecker.Kind                (Kind)
import           Saga.Language.Typechecker.Protocols           (Implementation,
                                                                ProtocolID)
import           Saga.Language.Typechecker.Qualification       (Multiplicity)
import           Saga.Language.Typechecker.Type                (Polymorphic,
                                                                Type)
import           Saga.Language.Typechecker.Variables


import           Control.Monad.RWS
import qualified Data.Set                                      as Set
import qualified Saga.Language.Typechecker.Qualification       as Q
import           Saga.Language.Typechecker.Solver.Substitution (Substitutable (..))
import qualified Saga.Language.Typechecker.Type                as T
import qualified Saga.Language.Typechecker.Variables           as V



data Constraint where
    Empty       :: Constraint
    Conjunction :: Constraint -> Constraint -> Constraint
    Equality    :: PolymorphicVar Evidence -> Item -> Item -> Constraint
    Impl        :: PolymorphicVar Evidence -> Item -> ProtocolID -> Constraint
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
    | Unification (PolymorphicVar Type)
    | Mono Type
    | Poly (Polymorphic Type)
    deriving (Show)

data Assumption
    = TypeLevel  T.Constraint
    | ValueLevel T.Constraint
    | Binding String (PolymorphicVar Type)
    deriving (Show)

data Evidence
    = Protocol Implementation
    | Coercion Mechanism
    deriving (Show, Eq, Ord)

type instance Restricted Evidence = ()


data Mechanism = Nominal | Structural deriving (Show, Eq, Ord)



