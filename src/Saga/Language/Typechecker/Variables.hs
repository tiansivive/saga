{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies    #-}



module Saga.Language.Typechecker.Variables where
import qualified Data.Kind as GHC


type Classifiable t = (Show (Classifier t), Eq (Classifier t), Ord (Classifier t))



data PolymorphicVar t where

  PolyVar          :: Classifiable t => String -> Classifier t -> PolymorphicVar t
  Skolem           :: Classifiable t => String -> Classifier t -> PolymorphicVar t
  Unification      :: Classifiable t => String -> Level -> Classifier t -> PolymorphicVar t
  Instantiation    :: Classifiable t => String -> PolymorphicVar t
  Evidence         :: Restricted t => String -> PolymorphicVar t

deriving instance Eq t    => Eq   (PolymorphicVar t)
deriving instance Ord t   => Ord  (PolymorphicVar t)
deriving instance Show t  => Show (PolymorphicVar t)

newtype Level = Level Int deriving (Show, Eq, Ord)

type family Classifier a  :: GHC.Type
type family VarType e a   :: GHC.Type
type family Restricted a  :: GHC.Constraint
