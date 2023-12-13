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



import           Control.Monad.RWS
import qualified Data.Map                                      as Map
import qualified Data.Set                                      as Set
import           Saga.Language.Core.Expr                       (Expr)
import qualified Saga.Language.Typechecker.Qualification       as Q

import           Saga.Language.Typechecker.Refinement.Liquid   (Liquid)

import           Saga.Language.Typechecker.Solver.Substitution (Substitutable (..))
import qualified Saga.Language.Typechecker.Type                as T

import           Saga.Language.Typechecker.Variables           (Variable)


data Constraint where
    Empty       :: Constraint
    Conjunction :: Constraint -> Constraint -> Constraint
    Equality    :: Variable Evidence -> Item -> Item -> Constraint
    Impl        :: Variable Evidence -> Item -> ProtocolID -> Constraint
    OneOf       :: Item -> Item -> Constraint
    Refined     :: Scope -> Item -> Liquid -> Constraint
    Resource    :: Item -> Multiplicity -> Constraint
    Consumed    :: Item -> Constraint
    Pure        :: Item -> Constraint
    Impure      :: Item -> Constraint
    Implication :: [Variable Type] -> [Assumption] -> Constraint -> Constraint
deriving instance Show Constraint
deriving instance Eq Constraint

data Item
    = Variable (Variable Item)
    -- | Skolem (Variable Type)
    | Mono Type
    | Poly (Polymorphic Type)
    deriving (Show, Eq)
data instance Variable Item where
    Skolem          :: Level -> Variable Type -> Variable Item
    Scoped          :: Level -> Variable Type -> Variable Item
    Unification     :: Level -> Variable Type -> Variable Item
    Instantiation   :: Variable Type -> Variable Item
deriving instance Show (Variable Item)
deriving instance Eq (Variable Item)
newtype Level = Level Int deriving (Show, Eq, Ord, Num)

data Evidence
    = Protocol Implementation
    | Coercion Mechanism
    deriving (Show, Eq, Ord)
data instance Variable Evidence where
    Evidence :: String -> Variable Evidence
deriving instance Show (Variable Evidence)
deriving instance Eq (Variable Evidence)
deriving instance Ord (Variable Evidence)

data Mechanism = Nominal | Structural deriving (Show, Eq, Ord)
type Witnessed = Map.Map (Variable Evidence) (Variable Evidence)

newtype Assumption
    = Assume Constraint
    deriving (Show, Eq)

type Scope = Map.Map (Variable Liquid) Item




instance Monoid Constraint where
    mempty = Empty
instance Semigroup Constraint where
    c <> c' = Conjunction c c'

instance Substitutable Constraint where
    type Target Constraint = Type
    apply sub (Conjunction c c')   = Conjunction (apply sub c) (apply sub c')
    apply sub (Equality ev it it') = Equality ev (apply sub it) (apply sub it')
    apply sub (Impl ev it prtcl)   = Impl ev (apply sub it) prtcl
    apply sub (Refined scope it liquid)   = Refined (apply sub scope) (apply sub it) liquid
    apply sub c                    = c

    ftv (Conjunction c c')        = ftv c <> ftv c'
    ftv (Equality ev it it')      = ftv it <> ftv it'
    ftv (Impl ev it prtcl)        = ftv it
    ftv (Refined scope it liquid) = ftv it
    ftv _                         = Set.empty

instance Substitutable Item where
    type Target Item = Type
    apply sub v@(Variable (Unification _ tvar))     = maybe v Mono $ Map.lookup tvar sub
    apply sub v@(Variable (Instantiation tvar))   = maybe v Mono $ Map.lookup tvar sub
    apply sub v@(Variable (Skolem _ tvar))          = maybe v Mono $ Map.lookup tvar sub
    apply sub v@(Variable (Scoped _ tvar))          = maybe v Mono $ Map.lookup tvar sub

    apply sub v@(Mono ty)         = Mono $ apply sub ty
    apply _ it                    = it

    ftv (Variable (Unification _ tvar)) = Set.singleton tvar
    ftv (Variable (Skolem _ tvar))      = Set.singleton tvar
    ftv (Variable (Scoped _ tvar))      = Set.singleton tvar
    ftv (Variable (Instantiation tvar)) = Set.singleton tvar
    ftv _                               = Set.empty





