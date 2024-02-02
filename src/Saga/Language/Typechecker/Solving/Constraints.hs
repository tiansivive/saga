{-# LANGUAGE TypeFamilies #-}
module Saga.Language.Typechecker.Solving.Constraints where
import           Data.Map                              (Map)
import           Saga.Language.Syntax.Liquids          (Liquid)
import           Saga.Language.Typechecker.Protocols   (Implementation,
                                                        ProtocolID)

import qualified Saga.Language.Syntax.Elaborated.Kinds as K
import           Saga.Language.Syntax.Elaborated.Types (Type)
import           Saga.Language.Syntax.Polymorphism     (Polymorphic)
import           Saga.Language.Typechecker.Variables   (Variable)

data Constraint where
    Empty       :: Constraint
    Conjunction :: Constraint -> Constraint -> Constraint
    Equality    :: Variable Constraint -> Item -> Item -> Constraint
    Impl        :: Variable Constraint -> Item -> ProtocolID -> Constraint
    OneOf       :: Item -> Item -> Constraint
    Refined     :: Scope -> Item -> Liquid -> Constraint
    Proof       :: Item -> Type -> Constraint
    Consumed    :: Item -> Constraint
    Pure        :: Item -> Constraint
    Impure      :: Item -> Constraint
    Implication :: [Variable Type] -> [Assumption] -> Constraint -> Constraint
    Evaluate    :: { result :: Type, cons :: Type, arg :: Type } -> Constraint
deriving instance Show Constraint



data Item
    = Var (Variable Type)
    | Mono Type
    | Poly Type
    | K K.Kind
    deriving (Show)

data instance Variable Constraint where
    Evidence :: String -> Variable Constraint
deriving instance Show (Variable Constraint)
deriving instance Eq (Variable Constraint)
deriving instance Ord (Variable Constraint)

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
