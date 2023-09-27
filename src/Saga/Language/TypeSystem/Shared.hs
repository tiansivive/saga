module Saga.Language.TypeSystem.Shared where

import qualified Data.Map                                           as Map
import           Saga.Language.TypeSystem.Environment
import qualified Saga.Language.TypeSystem.Types       as T
import           Saga.Language.TypeSystem.Types

import           Control.Monad.RWS









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


