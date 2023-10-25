{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Typechecker.Type where
import           Data.Typeable                                 (Typeable)
import qualified Saga.Language.Core.Liquid                     as Liquid
import           Saga.Language.Core.Literals
import           Saga.Language.Typechecker.Kind                (Kind)
import qualified Saga.Language.Typechecker.Qualification       as Q
import           Saga.Language.Typechecker.TypeExpr            (TypeExpr)
import           Saga.Language.Typechecker.Variables


import qualified Data.Map                                      as Map
import qualified Data.Set                                      as Set
import           Saga.Language.Typechecker.Qualification       (Qualified (..))
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
    Var         :: PolymorphicVar Type -> Type
    Closure     :: [PolymorphicVar Type] -> TypeExpr -> Scope -> Type
    Void        :: Type
    Any         :: Type


data Scope = Scope
  { types     :: Map.Map String (Polymorphic Type)
  , kinds     :: Map.Map String (Polymorphic Kind)
  , dataTypes :: Map.Map String DataType
  , tags      :: Map.Map String (Polymorphic Type)
  } deriving (Show, Eq, Ord)


type Constraint = Q.Constraint Type


data Tycon = Tycon String Kind deriving (Show, Eq, Ord)
data Scheme t = Forall [PolymorphicVar t] (Qualified t) deriving (Show, Eq, Ord)
type Polymorphic = Scheme

data DataType = DataType { userType :: Polymorphic Type, members:: Map.Map String Constructor } deriving (Show, Eq, Ord)
data Constructor = DCons { constructor :: Polymorphic Type, cdata :: Polymorphic Type }  deriving (Show, Eq, Ord)


deriving instance Show Type
deriving instance Eq Type
deriving instance Ord Type


instance Substitutable Type Type where
  --apply s t | trace ("Applying type sub\n\t" ++ show s ++ "\n\t" ++ show t) False = undefined

  apply s t@(Var v)                      = Map.findWithDefault t v s
  apply s (Tuple elems)                  = Tuple $ apply s elems
  apply s (Record pairs)                 = Record $ apply s pairs
  apply s (Union elems)                  = Union $ apply s elems
  apply s (Applied cons arg)             = Applied (apply s cons) (apply s arg)
  --apply s (TClosure params body env)      = TClosure params (apply s body) (apply s env)
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

instance {-# OVERLAPS #-} Substitutable (Qualified Type) Type where
    apply s (cs :=> t) = apply s cs :=> apply s t
    ftv (cs :=> t) = ftv cs `Set.union` ftv t


instance Substitutable Constraint Type where
  apply s (t `Q.Implements` prtcl) = apply s t `Q.Implements` prtcl
  apply s (Q.Resource mul t)       = Q.Resource mul $ apply s t
  apply s (Q.Refinement expr t)    = Q.Refinement expr $ apply s t
  apply s (Q.Pure t)               = Q.Pure $ apply s t

  ftv (t `Q.Implements` prtcl) = ftv t
  ftv (Q.Resource mul t)       = ftv t
  ftv (Q.Refinement expr t)    = ftv t
  ftv (Q.Pure t)               = ftv t
