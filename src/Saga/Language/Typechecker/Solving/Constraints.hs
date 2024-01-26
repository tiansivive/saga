{-# LANGUAGE TypeFamilies #-}
module Saga.Language.Typechecker.Solving.Constraints where
import           Data.Map                              (Map)
import           Saga.Language.Syntax.Liquids          (Liquid)
import           Saga.Language.Typechecker.Protocols   (Implementation,
                                                        ProtocolID)

import           Saga.Language.Syntax.Elaborated.Types (Type)
import           Saga.Language.Syntax.Polymorphism     (Polymorphic)
import           Saga.Language.Typechecker.Variables   (Variable)

data Constraint where
    Empty       :: Constraint
    Conjunction :: Constraint -> Constraint -> Constraint
    Equality    :: Variable Evidence -> Item -> Item -> Constraint
    Impl        :: Variable Evidence -> Item -> ProtocolID -> Constraint
    OneOf       :: Item -> Item -> Constraint
    Refined     :: Scope -> Item -> Liquid -> Constraint
    Proof       :: Item -> Type -> Constraint
    Consumed    :: Item -> Constraint
    Pure        :: Item -> Constraint
    Impure      :: Item -> Constraint
    Implication :: [Variable Type] -> [Assumption] -> Constraint -> Constraint
deriving instance Show Constraint



data Item
    = Var (Variable Type)
    | Mono Type
    | Poly (Polymorphic Type)
    deriving (Show)

data instance Variable Evidence where
    Evidence :: String -> Variable Evidence
deriving instance Show (Variable Evidence)
deriving instance Eq (Variable Evidence)
deriving instance Ord (Variable Evidence)

data Evidence
    = Protocol Implementation
    | Coercion Mechanism
    deriving (Show, Eq, Ord)
data Mechanism = Nominal | Structural deriving (Show, Eq, Ord)

newtype Assumption
    = Assume Constraint
    deriving (Show)


type Scope = Map (Variable Liquid) Item



instance Monoid Constraint where
    mempty = Empty
instance Semigroup Constraint where
    c <> c' = Conjunction c c'
