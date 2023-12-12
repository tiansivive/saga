{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Typechecker.Kind where
import qualified Data.Map                                      as Map
import qualified Data.Set                                      as Set
import qualified Saga.Language.Typechecker.Qualification       as Q
import           Saga.Language.Typechecker.Qualification       (Qualified (..))
import           Saga.Language.Typechecker.Solver.Substitution (Substitutable (..))
import qualified Saga.Language.Typechecker.Variables           as Var
import           Saga.Language.Typechecker.Variables           (Classifiable,
                                                                Variable)


data Kind
  = Type
  | Kind
  | Var (Variable Kind)
  | Arrow Kind Kind
  | Protocol Kind
  | Constraint Kind
  | Data String Kind
    deriving (Show, Eq, Ord)

data instance Variable Kind where
  Poly          :: Classifiable Kind => String -> Var.Classifier Kind -> Variable Kind
  Unification   :: Classifiable Kind => String -> Var.Classifier Kind -> Variable Kind

deriving instance Show (Variable Kind)
deriving instance Ord (Variable Kind)
deriving instance Eq (Variable Kind)

instance Substitutable Kind where
    type Target Kind = Kind
    apply s (Protocol k) = Protocol $ apply s k
    apply s (Constraint k) = Constraint $ apply s k
    apply s (Data id k) = Data id $ apply s k

    apply s k@(Var v) = Map.findWithDefault k v s
    apply s (inTy `Arrow` outTy)           = in' `Arrow` out'
      where
        in' = apply s inTy
        out' = apply s outTy

    apply _ k = k

    ftv ty = case ty of
      Data _ k     -> ftv k
      Constraint k -> ftv k
      Protocol k   -> ftv k
      Var v        -> Set.singleton v
      k `Arrow` k' -> ftv k `Set.union` ftv k'
      _            -> Set.empty

instance {-# OVERLAPS #-} Substitutable (Qualified Kind) where
    type Target (Qualified Kind) = Target Kind
    -- Polymorphic Kinds do not have Constraints
    apply s (given :=> t) = given :=> apply s t
    ftv (_ :=> t) = ftv t

