{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Saga.Language.Typechecker.Type where
import           Data.Typeable                                 (Typeable)
import           Saga.Language.Core.Literals
import           Saga.Language.Typechecker.Kind                (Kind)
import qualified Saga.Language.Typechecker.Qualification       as Q
import qualified Saga.Language.Typechecker.Refinement.Liquid   as Liquid
import           Saga.Language.Typechecker.TypeExpr            (TypeExpr)
import           Saga.Language.Typechecker.Variables


import qualified Data.Map                                      as Map
import qualified Data.Set                                      as Set
import           Saga.Language.Typechecker.Qualification       (Given (..),
                                                                Qualified (..))
import           Saga.Language.Typechecker.Solver.Substitution (Subst,
                                                                Substitutable (..))


data Type where
    Singleton   :: Literal -> Type
    Tuple       :: [Type] -> Type
    Record      :: [(String, Type)] -> Type
    Union       :: [Type] -> Type
    Arrow       :: Type -> Type -> Type
    Data        :: String -> Kind -> Type
    Applied     :: Type -> Type -> Type
    Var         :: Variable Type -> Type
    Closure     :: [Variable Type] -> TypeExpr -> Scope -> Type
    Void        :: Type
    Any         :: Type


deriving instance Show Type
deriving instance Eq Type
deriving instance Ord Type

data instance Variable Type where
  Poly              :: Classifiable Type => String -> Classifier Type -> Variable Type
  Existential       :: Classifiable Type => String -> Classifier Type -> Variable Type
  Local             :: Classifiable Type => String -> Classifier Type -> Variable Type

deriving instance Show (Variable Type)
deriving instance Ord (Variable Type)
deriving instance Eq (Variable Type)

classifier :: Variable Type -> Classifier Type
classifier (Poly _ c)        = c
classifier (Existential _ c) = c
classifier (Local _ c)       = c

data Scope = Scope
  { types :: Map.Map String (Polymorphic Type)
  , kinds :: Map.Map String (Polymorphic Kind)
, tags    :: [Tag]
  } deriving (Show, Eq, Ord)


data Scheme t = Forall [Variable t] (Qualified t)
deriving instance (Show t, Show (Variable t)) => Show (Scheme t)
deriving instance (Eq t, Eq (Variable t)) => Eq (Scheme t)
deriving instance (Ord t, Ord (Variable t)) => Ord (Scheme t)
type Polymorphic = Scheme

data DataType = DataType { tycon :: Tycon, definition :: Polymorphic Type } deriving (Show, Eq, Ord)
data Tycon = Tycon String Kind deriving (Show, Eq, Ord)
data Tag = Constructor
  { name        :: String
  , constructor :: Polymorphic Type
  , package     :: Polymorphic Type
  , target      :: DataType
  } deriving (Show, Eq, Ord)


instance Substitutable Type where
  type Target Type = Type
  --apply s t | trace ("Applying type sub\n\t" ++ show s ++ "\n\t" ++ show t) False = undefined

  apply s t@(Var v)                      = Map.findWithDefault t v s
  apply s (Tuple elems)                  = Tuple $ apply s elems
  apply s (Record pairs)                 = Record $ apply s pairs
  apply s (Union elems)                  = Union $ apply s elems
  apply s (Applied cons arg)             = Applied (apply s cons) (apply s arg)
  --apply s (Closure params body env)      = Closure params (apply s body) (apply s env)
  apply s (inTy `Arrow` outTy)           = in' `Arrow` out'
    where
      in' = apply s inTy
      out' = apply s outTy

  apply _ ty = ty

  ftv ty = case ty of
    Var id             -> Set.singleton id
    Tuple elems        -> ftv elems
    Record pairs       -> ftv pairs
    Union tys          -> ftv tys
    cons `Applied` arg -> ftv cons `Set.union` ftv arg
    t `Arrow` t'       -> ftv t `Set.union` ftv t'
    _                  -> Set.empty
  --ftv (TClosure params body _) = ftv body

instance {-# OVERLAPS #-} Substitutable (Qualified Type) where
    type Target (Qualified Type) = Target Type
    apply s (bindings :| cs :=> t) = apply s bindings :| apply s cs :=> apply s t
    ftv (bindings :| cs :=> t) = ftv bindings <> ftv cs <> ftv t


type Constraint = Q.Constraint Type
instance Substitutable Constraint where
  type Target Constraint = Target Type
  apply s (t `Q.Implements` prtcl) = apply s t `Q.Implements` prtcl
  apply s (Q.Resource mul t)       = Q.Resource mul $ apply s t
  apply s (Q.Refinement bs expr t) = Q.Refinement (apply s bs) expr $ apply s t
  apply s (Q.Pure t)               = Q.Pure $ apply s t

  ftv (t `Q.Implements` prtcl) = ftv t
  ftv (Q.Resource mul t)       = ftv t
  ftv (Q.Refinement bs expr t) = ftv t
  ftv (Q.Pure t)               = ftv t
