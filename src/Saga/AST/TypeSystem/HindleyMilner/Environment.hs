module Saga.AST.TypeSystem.HindleyMilner.Environment where

import           Control.Monad.Except
import           Control.Monad.State.Lazy
import qualified Data.Map                                as Map
import           Saga.AST.TypeSystem.HindleyMilner.Types



type TVar = String
type Alias = String
data Scheme = Scheme [TVar] Type

data TypeEnv = Env { typeVars :: Map.Map TVar Scheme, typeAliases :: Map.Map Alias TypeExpr, count :: Int }



type Infer = StateT TypeEnv (Except TypeError)
type Subst = Map.Map TVar Type

nullSubst :: Subst
nullSubst = Map.empty

data TypeError
  = UnificationFail Type  Type
  | InfiniteType TVar Type
  | UnboundVariable String
  | UndefinedIdentifier Alias
  | UnexpectedType String


fresh :: Infer Type
fresh = do
  s <- get
  put s{count = count s + 1}
  return $ TVar $ (letters !! count s)


letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']



