{-# LANGUAGE MultiParamTypeClasses #-}

module Saga.AST.TypeSystem.HindleyMilner.Environment where

import           Control.Monad.Except
import           Control.Monad.State.Lazy                hiding (get, modify)
import qualified Control.Monad.State.Lazy                as ST
import           Control.Monad.Trans.RWS                 (RWST, get, local,
                                                          modify, tell)
import qualified Data.Map                                as Map
import           Saga.AST.TypeSystem.HindleyMilner.Types



type UnificationVar = String
type Alias = String
type Name = String
data Scheme = Scheme [UnificationVar] Type deriving (Show, Eq)

data TypeEnv = Env { unifier :: Map.Map UnificationVar Scheme, aliases :: Map.Map Alias Type }
  deriving (Show)

newtype InferenceState = IST { count :: Int }

type Infer = RWST TypeEnv [IConstraint] InferenceState (Except InferenceError)





data IConstraint
  = Empty
  | Equals Type Type
  | Protocol Name [Type]
  | Conjunction IConstraint IConstraint
  | Implication [UnificationVar] IConstraint IConstraint
  -- | Subtype Type Type -- Is this at all needed? probably not

data InferenceError
  = UnboundVariable String
  | UndefinedIdentifier Alias
  | UnexpectedType String
  | UnificationMismatch [Type] [Type]
  | UnificationFail Type Type
  | InfiniteType UnificationVar Type
  | SubtypeFailure Type Type
  | Fail String
  deriving (Show, Eq)


emit :: IConstraint -> Infer ()
emit = tell . pure


empty :: TypeEnv
empty = Env { unifier = Map.empty, aliases = builtInFns }

initState :: InferenceState
initState = IST { count = 0 }

numProtocol :: Type
numProtocol = TRecord [("+", TVar "a" `TArrow` TVar "a" `TArrow` TVar "a")]

builtInFns :: Map.Map Alias Type
builtInFns = Map.fromList
  [ ("+", TConstrained  [ TVar "a" `Implements` numProtocol] ( TVar "a" `TArrow` TVar "a" `TArrow` TVar "a"))
  , ("-",  TConstrained [ TVar "a" `Implements` numProtocol] ( TVar "a" `TArrow` TVar "a" `TArrow` TVar "a"))
  , ("*", TConstrained  [ TVar "a" `Implements` numProtocol] ( TVar "a" `TArrow` TVar "a" `TArrow` TVar "a"))
  , ("/", TConstrained  [ TVar "a" `Implements` numProtocol] ( TVar "a" `TArrow` TVar "a" `TArrow` TVar "a"))
  ]

union :: TypeEnv -> TypeEnv -> TypeEnv
(Env unifier aliases) `union` (Env unifier' aliases' ) = Env
  { unifier = Map.union unifier unifier'
  , aliases = Map.union aliases aliases'
}

extend :: TypeEnv -> (UnificationVar, Scheme) -> TypeEnv
extend e@(Env unifier aliases) (var, scheme) = e{ unifier = Map.insert var scheme unifier }


scoped :: Infer a -> (UnificationVar, Scheme) -> Infer a
scoped m (var, scheme) = do
  let scoped' = local $ \env -> env `extend` (var, scheme)
  scoped' m



fresh :: Infer Type
fresh = do
  modify $ \s -> s{count = count s + 1}
  s <- get
  return $ TVar $ "t" ++ show ([1..] !! count s)


letters :: [String]
letters = [1..] >>= flip replicateM ['α'..'ω']



