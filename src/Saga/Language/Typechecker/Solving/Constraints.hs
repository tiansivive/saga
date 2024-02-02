{-# LANGUAGE TypeFamilies #-}
module Saga.Language.Typechecker.Solving.Constraints where
import           Data.Map                               (Map)
import           Saga.Language.Syntax.Liquids           (Liquid)
import           Saga.Language.Typechecker.Protocols    (Implementation,
                                                         ProtocolID)

import qualified Saga.Language.Syntax.Elaborated.Kinds  as K
import qualified Saga.Language.Syntax.Elaborated.Types  as T
import           Saga.Language.Syntax.Elaborated.Types  (Type)
import           Saga.Language.Syntax.Polymorphism      (Polymorphic)


import qualified Saga.Language.Syntax.AST               as NT (NodeType (..))
import           Saga.Language.Syntax.AST               (AST,
                                                         Phase (Elaborated))

import qualified Data.Map                               as Map
import qualified Data.Set                               as Set

import           Saga.Language.Typechecker.Substitution (Substitutable (..))
import           Saga.Language.Typechecker.Variables    (Variable)
import           Saga.Utils.Operators                   ((|>), (||>))

import           Saga.Language.Syntax.Reduced.Types     (TypeExpr)
import           Saga.Language.Typechecker.Traversals

data Constraint where
    Empty       :: Constraint
    Conjunction :: Constraint -> Constraint -> Constraint
    Equality    :: Variable Constraint -> Item -> Item -> Constraint
    Impl        :: Variable Constraint -> Type -> ProtocolID -> Constraint
    OneOf       :: { source :: Type, union :: Type } -> Constraint
    Refined     :: Bindings -> Type -> Liquid -> Constraint
    Proof       :: { source :: Type, narrowed :: Type } -> Constraint
    -- Consumed    :: Item -> Constraint
    -- Pure        :: Item -> Constraint
    -- Impure      :: Item -> Constraint
    Implication :: [Variable Type] -> [Assumption] -> Constraint -> Constraint
    Evaluate    :: { result :: Type, expr :: Expression } -> Constraint
deriving instance Show Constraint
deriving instance Eq Constraint

data Expression
    = Application Type Type
    | Match TypeExpr Scope
    deriving (Show, Eq)

data Scope = Scope
    { types :: Map String (AST Elaborated NT.Type)
    , kinds :: Map String (AST Elaborated NT.Kind)
    } deriving (Show, Eq)

data Item
    = Ty Type
    | K K.Kind
    deriving (Show, Eq)

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
    deriving (Show, Eq)


type Bindings = Map (Variable Liquid) Type



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

    apply sub v@(Ty (T.Var tvar)) = maybe v Ty $ Map.lookup tvar sub
    apply sub v@(Ty t)            = Ty $ apply sub t
    apply _ ty                    = ty

    ftv (Ty t) = ftv t
    ftv _      = Set.empty

