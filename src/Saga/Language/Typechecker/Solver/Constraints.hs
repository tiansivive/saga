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

import           Data.Array                                    (elems)
import           Saga.Language.Core.Literals                   (Literal)
import           Saga.Language.Typechecker.Variables           (Variable)
import           Saga.Utils.Operators                          ((|>), (||>))


data Constraint where
    Empty       :: Constraint
    Conjunction :: Constraint -> Constraint -> Constraint
    Equality    :: Variable Evidence -> Item -> Item -> Constraint
    Impl        :: Variable Evidence -> Item -> ProtocolID -> Constraint
    OneOf       :: Item -> Item -> Constraint
    Refined     :: Scope -> Item -> Liquid -> Constraint
    --Proof       :: Item -> Literal -> Constraint
    Resource    :: Item -> Multiplicity -> Constraint
    Consumed    :: Item -> Constraint
    Pure        :: Item -> Constraint
    Impure      :: Item -> Constraint
    Implication :: [Variable Type] -> [Assumption] -> Constraint -> Constraint
deriving instance Show Constraint
deriving instance Eq Constraint

data Item
    = Skolem (Variable Type)
    | Scoped (Variable Type)
    | Unification (Variable Type)
    | Instantiation (Variable Type)
    | Mono Type
    | Poly (Polymorphic Type)
    deriving (Show, Eq)
-- data instance Variable Item where
--     Skolem          :: Variable Type -> Variable Item
--     Scoped          :: Variable Type -> Variable Item
--     Unification     :: Variable Type -> Variable Item
--     Instantiation   :: Variable Type -> Variable Item
-- deriving instance Show (Variable Item)
-- deriving instance Eq (Variable Item)

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
    apply sub (Refined scope it liquid)   = Refined scope (apply sub it) liquid
    apply sub c                    = c

    ftv (Conjunction c c')        = ftv c <> ftv c'
    ftv (Equality ev it it')      = ftv it <> ftv it'
    ftv (Impl ev it prtcl)        = ftv it
    ftv (Refined scope it liquid) = ftv it <> (Map.elems scope ||> fmap ftv |> mconcat)
    ftv _                         = Set.empty

instance Substitutable Item where
    type Target Item = Type
    apply sub v@(Unification tvar)   = maybe v Mono $ Map.lookup tvar sub
    apply sub v@(Instantiation tvar) = maybe v Mono $ Map.lookup tvar sub
    apply sub v@(Skolem tvar)        = maybe v Mono $ Map.lookup tvar sub
    apply sub v@(Scoped tvar)        = maybe v Mono $ Map.lookup tvar sub

    apply sub v@(Mono ty)            = Mono $ apply sub ty
    apply _ it                       = it

    ftv (Unification  tvar)  = Set.singleton tvar
    ftv (Skolem  tvar)       = Set.singleton tvar
    ftv (Scoped tvar)        = Set.singleton tvar
    ftv (Instantiation tvar) = Set.singleton tvar
    ftv _                    = Set.empty





