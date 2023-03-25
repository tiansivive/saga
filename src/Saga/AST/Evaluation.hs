
module Saga.AST.Evaluation where

import           Saga.AST.Syntax            (Definition (..), Expr (..),
                                             Literal (..), Name (..))

import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map                   as Map

import           Control.Monad.State.Lazy

data Value a
  = VInt Int
  | VBool Bool
  | VString String
  | VList [Value a]
  | VTuple [Value a]
  | VRecord [(String, Value a)]
  | VClosure [Name a] (Expr a) (Env a)
  deriving (Show)

type Env a = Map.Map String (Value a)

type EvalState a = StateT (Env a) (Either String) (Value a)


eval :: Expr a -> EvalState a
eval (Lit l) = evalLit l
eval (Declaration (Def _ (Name _ name) e)) =
  let name' = BS.unpack name in do
      val <- eval e
      modify $ Map.insert name' val
      return val
eval (Block _ defs e) =
  let
    eval' def = eval $ Declaration def
    eval'' = do
      mapM_ eval' defs
      eval e
  in do
      env <- get
      lift $ evalStateT eval'' env

eval (Lambda _ args body) = do
  VClosure args body <$> get
eval (Identifier (Name _ name)) =
  let
    name' = BS.unpack name
    errorMsg = "Undefined identifier " <> name'
  in do
    env <- get
    let val = Map.lookup name' env
    lift $ fromMaybe errorMsg val

eval (FnApp _ fnExpr argExprs) = do
  vals <- mapM eval argExprs
  closure <- eval fnExpr
  env <- get

  case closure of
    (VClosure paramNames bodyExpr capturedEnv) ->
      if length paramNames == length vals then
        let
          pairs = zip paramNames vals
          insert' (Name _ name, v) = Map.insert (BS.unpack name) v
          env' = foldr insert' capturedEnv pairs
          env'' = Map.union env env'
        in lift $ evalStateT (eval bodyExpr) env''

      else lift $ Left "Wrong number of params"
    _ -> lift $ Left "Cannot apply this expression"



evalLit :: Literal a -> EvalState a
evalLit (LInt _ int)    = return $ VInt int
evalLit (LBool _ bool)  = return $ VBool bool
evalLit (LString _ str) = return $ VString $ BS.unpack str
evalLit (LList _ list) = do
  vals <- mapM eval list
  return $ VList vals
evalLit (LTuple _ tuple)  = do
  vals <- mapM eval tuple
  return $ VTuple vals
evalLit (LRecord _ record) = do
  vals <- mapM evalPair record
  return $ VRecord vals
  where
    evalPair (Name _ name, e) = do
      v <- eval e
      return (BS.unpack name, v)



runEvaluation :: Expr a -> Either String (Value a, Env a)
runEvaluation e = runStateT (eval e) Map.empty




fromMaybe :: a -> Maybe b -> Either a b
fromMaybe _ (Just b) = Right b
fromMaybe a Nothing  = Left a


