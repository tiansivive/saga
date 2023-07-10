{-# LANGUAGE MultiParamTypeClasses #-}

module Saga.AST.TypeSystem.HindleyMilner.Environment where

import           Control.Monad.Except
import           Control.Monad.State.Lazy                hiding (get, modify)
import qualified Control.Monad.State.Lazy                as ST
import qualified Data.Map                                as Map
import           Saga.AST.TypeSystem.HindleyMilner.Types



type TVar = String
type Alias = String
data Scheme = Scheme [TVar] Type deriving (Show, Eq)

data TypeEnv = Env { typeVars :: Map.Map TVar Scheme, typeAliases :: Map.Map Alias Type, count :: Int }
  deriving (Show)


type Infer = StateT [TypeEnv] (Except TypeError)
type Subst = Map.Map TVar Type

newtype Protocol a b = Protocol [(String, a -> b)]

empty :: TypeEnv
empty = Env { typeVars = Map.empty, typeAliases = builtInFns, count = 0 }


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
(Env vars aliases count) `union` (Env vars' aliases' count') = Env
  { typeVars = Map.union vars vars'
  , typeAliases = Map.union aliases aliases'
  , count = count + count'
}

get :: Infer TypeEnv
get = foldr union empty <$> ST.get



modify :: (TypeEnv -> TypeEnv) -> Infer ()
modify f = ST.modify push
  where
    push []       = [f empty]
    push [te]     = [f te]
    push (_:rest) = push rest

modifyM :: (TypeEnv -> Infer TypeEnv) -> Infer ()
modifyM f = do
  env <- ST.get
  env' <- push env
  put env'
  where
    push []       = mapM f [empty]
    push [te]     = mapM f [te]
    push (_:rest) = push rest

nullSubst :: Subst
nullSubst = Map.empty

data TypeError
  = UnificationFail Type Type
  | InfiniteType TVar Type
  | UnboundVariable String
  | UndefinedIdentifier Alias
  | UnexpectedType String
  | SubtypeFailure Type Type

  deriving (Show, Eq)


fresh :: Infer Type
fresh = do

  modify $ \s -> s{count = count s + 1}
  s <- get
  return $ TVar $ (letters !! count s)


letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']



