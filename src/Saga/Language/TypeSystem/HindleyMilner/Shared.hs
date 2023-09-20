module Saga.Language.TypeSystem.HindleyMilner.Shared where

import qualified Data.Map                                           as Map
import           Saga.Language.TypeSystem.HindleyMilner.Environment
import qualified Saga.Language.TypeSystem.HindleyMilner.Types       as T
import           Saga.Language.TypeSystem.HindleyMilner.Types

import           Control.Monad.RWS




empty :: InferenceEnv
empty = Env Map.empty builtInFns




builtInFns :: Map.Map Alias TypeExpr
builtInFns =
  Map.fromList
    [ ("+", binaryNumTypeExpr),
      ("-", binaryNumTypeExpr),
      ("*", binaryNumTypeExpr),
      ("/", binaryNumTypeExpr)
    ]
    where
      var = "a"
      tvar = TVar $ Tyvar var KType
      binaryNumTypeExpr = TQualified $ [tvar `T.Implements` "Num"] :=> TLambda [var] (TAtom $ tvar `TArrow` (tvar `TArrow` tvar))



extend :: InferenceEnv -> (UnificationVar, TypeExpr) -> InferenceEnv
extend e@(Env unifier aliases) (var, tyExpr) = e {unificationVars = Map.insert var tyExpr unifier}





letters :: [String]
letters = [1 ..] >>= flip replicateM ['α' .. 'ω']

mkIConstraint :: Constraint -> IConstraint
mkIConstraint (ty `T.Implements` protocol) = ImplCons $ ty `IP` protocol


implementationConstraints :: [IConstraint] -> [ImplConstraint]
implementationConstraints cs = [ ip | ImplCons ip <- cs ]

implementationTy :: ImplConstraint -> Type
implementationTy (ty `IP` p) = ty

implementationP :: ImplConstraint -> String
implementationP (ty `IP` p) = p


