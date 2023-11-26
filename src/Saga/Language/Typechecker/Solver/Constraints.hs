{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Typechecker.Solver.Constraints where
import           Saga.Language.Typechecker.Kind                (Kind)
import           Saga.Language.Typechecker.Protocols           (Implementation,
                                                                ProtocolID)
import           Saga.Language.Typechecker.Qualification       (Multiplicity)

import           Saga.Language.Typechecker.Type                (Polymorphic,
                                                                Type)
import           Saga.Language.Typechecker.Variables           hiding
                                                               (Unification)


import           Control.Monad.RWS
import qualified Data.Map                                      as Map
import qualified Data.Set                                      as Set
import           Saga.Language.Core.Expr                       (Expr)
import qualified Saga.Language.Typechecker.Qualification       as Q
import qualified Saga.Language.Typechecker.Refinement.Liquid   as Liquid
import           Saga.Language.Typechecker.Solver.Substitution (Substitutable (..))
import qualified Saga.Language.Typechecker.Type                as T
import qualified Saga.Language.Typechecker.Variables           as V hiding
                                                                    (Unification)



data Constraint where
    Empty       :: Constraint
    Conjunction :: Constraint -> Constraint -> Constraint
    Equality    :: PolymorphicVar Evidence -> Item -> Item -> Constraint
    Impl        :: PolymorphicVar Evidence -> Item -> ProtocolID -> Constraint
    OneOf       :: Item -> Item -> Constraint
    Refined     :: Item -> Liquid.Liquid -> Constraint
    Resource    :: Item -> Multiplicity -> Constraint
    Consumed    :: Item -> Constraint
    Pure        :: Item -> Constraint
    Impure      :: Item -> Constraint
    Implication :: [PolymorphicVar Type] -> [Assumption] -> Constraint -> Constraint
deriving instance Show Constraint
deriving instance Eq Constraint

data Item
    = Skolem String Kind
    | Unification (PolymorphicVar Type)
    | Mono Type
    | Poly (Polymorphic Type)
    deriving (Show, Eq)

data Evidence
    = Protocol Implementation
    | Coercion Mechanism
    deriving (Show, Eq, Ord)
type instance Restricted Evidence = ()

data Mechanism = Nominal | Structural deriving (Show, Eq, Ord)

type Witnessed = Map.Map (PolymorphicVar Evidence) (PolymorphicVar Evidence)


data Assumption
    = Assume Expr (PolymorphicVar Type)

    deriving (Show, Eq)






instance Monoid Constraint where
    mempty = Empty
instance Semigroup Constraint where
    c <> c' = Conjunction c c'

instance Substitutable Constraint Type where
    apply sub (Conjunction c c')   = Conjunction (apply sub c) (apply sub c')
    apply sub (Equality ev it it') = Equality ev (apply sub it) (apply sub it')
    apply sub (Impl ev it prtcl)   = Impl ev (apply sub it) prtcl
    apply sub c                    = c

    ftv (Conjunction c c')   = ftv c <> ftv c'
    ftv (Equality ev it it') = ftv it <> ftv it'
    ftv (Impl ev it prtcl)   = ftv it
    ftv _                    = Set.empty

instance Substitutable Item Type where
    apply sub v@(Unification uvar) = maybe v Mono $ Map.lookup uvar sub
    apply sub v@(Mono ty)          = Mono $ apply sub ty
    apply _ it                     = it

    ftv (Unification uvar) = Set.singleton uvar
    ftv _                  = Set.empty





