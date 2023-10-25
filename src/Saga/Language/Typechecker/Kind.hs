module Saga.Language.Typechecker.Kind where
import qualified Data.Map                                     as Map
import qualified Data.Set                                     as Set
import qualified Saga.Language.Typechecker.Qualification      as Q
import           Saga.Language.Typechecker.Qualification      (Qualified (..))
import           Saga.Language.Typechecker.Solver.Unification (Substitutable (..))
import           Saga.Language.Typechecker.Variables          (PolymorphicVar)


data Kind
  = Type
  | Kind
  | Var (PolymorphicVar Kind)
  | Arrow Kind Kind
  | Protocol Kind
  | Constraint Kind
  | Data String Kind
    deriving (Show, Eq, Ord)

instance Substitutable Kind Kind where
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

instance {-# OVERLAPS #-} Substitutable (Qualified Kind) Kind where
    -- Polymorphic Kinds do not have Constraints
    apply s (_ :=> t) = [] :=> apply s t
    ftv (_ :=> t) = ftv t

