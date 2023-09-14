module Saga.Language.TypeSystem.HindleyMilner.Shared where

import qualified Data.Map                                           as Map
import           Saga.Language.TypeSystem.HindleyMilner.Environment
import qualified Saga.Language.TypeSystem.HindleyMilner.Types       as T
import           Saga.Language.TypeSystem.HindleyMilner.Types

import           Control.Monad.RWS


emit :: IConstraint -> Infer ()
emit = tell . pure

empty :: InferenceEnv
empty = Env Map.empty builtInFns

initState :: InferenceState
initState = IST {count = 0}




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

-- builtInFns :: Map.Map Alias Scheme
-- builtInFns =
--   Map.fromList
--     [ ("+", Scheme [var] ([tvar `T.Implements` "Num"] :=> tvar `TArrow` (tvar `TArrow` tvar))),
--       ("-", Scheme [var] ([tvar `T.Implements` "Num"] :=> tvar `TArrow` (tvar `TArrow` tvar))),
--       ("*", Scheme [var] ([tvar `T.Implements` "Num"] :=> tvar `TArrow` (tvar `TArrow` tvar))),
--       ("/", Scheme [var] ([tvar `T.Implements` "Num"] :=> tvar `TArrow` (tvar `TArrow` tvar)))
--     ]
--     where
--       var = Tyvar "a" KType
--       tvar = TVar var


union :: InferenceEnv -> InferenceEnv -> InferenceEnv
(Env unifier aliases) `union` (Env unifier' aliases') =
  Env
    { unificationVars = Map.union unifier unifier',
      aliases = Map.union aliases aliases'
    }

extend :: InferenceEnv -> (UnificationVar, TypeExpr) -> InferenceEnv
extend e@(Env unifier aliases) (var, tyExpr) = e {unificationVars = Map.insert var tyExpr unifier}

scoped :: Infer a -> (UnificationVar, TypeExpr) -> Infer a
scoped m (var, tyExpr) = do
  let scoped' = local $ \env -> env `extend` (var, tyExpr)
  scoped' m

fresh :: Kind -> Infer Type
fresh k = do
  modify $ \s -> s {count = count s + 1}
  s <- get
  let v = "t" ++ show ([1 ..] !! count s)
  return $ TVar $ Tyvar v k

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


