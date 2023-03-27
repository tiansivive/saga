
module Saga.AST.Evaluation where

import           Saga.AST.Syntax            (Definition (..), Expr (..),
                                             Literal (..), Name (..))

import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map                   as Map

import           Control.Monad.State.Lazy
import           Data.List                  (findIndex)
import           Debug.Trace                (traceM)



builtInEnv :: [String]
builtInEnv = ["+", "-","*", "^", "/", "%", "==", "!=", "||", "&&"]

data Value a
  = VInt Int
  | VBool Bool
  | VString String
  | VList [Value a]
  | VTuple [Value a]
  | VRecord [(String, Value a)]
  | VClosure [Name a] (Expr a) (Env a)
  | BuiltIn String
  | Void
  deriving (Show, Eq)

type Env a = Map.Map String (Value a)

type EvalState a = StateT (Env a) (Either String) (Value a)



eval :: (Eq a, Show a) => Expr a -> EvalState a
eval (Lit l) = evalLit l
eval (Parens _ e) = eval e
eval (Return _ e) = eval e

eval (Identifier (Name _ name)) =
    let
      errorMsg = "Undefined identifier " <> name
    in if name `elem` builtInEnv
      then return $ BuiltIn name
      else  do
        env <- get
        let val = Map.lookup name env
        lift $ fromMaybe errorMsg val

eval (Declaration (Def _ (Name _ name) e)) = do
      val <- eval e
      modify $ Map.insert name val
      return val

eval (Clause _ defs e) =
  let
    eval' def = eval $ Declaration def
    eval'' = do
      mapM_ eval' defs
      eval e
  in do
      env <- get
      lift $ evalStateT eval'' env

eval (Flow _ cond onTrue onFalse) = do
  val <- eval cond
  case val of
    (VBool True) -> eval onTrue
    (VBool False) -> eval onFalse
    _ -> lift $ Left "Could not evaluate non boolean expression as a if condition"

eval (Block _ exprs) = do
  traceM $ "rest: " <> show rest
  traceM $ "exprs': " <> show exprs'
  mapM_ eval exprs'
  eval' rest
    where
      returnStmt e | (Return _ _) <- e = True
                   | otherwise         = False
      (exprs', rest) = break returnStmt exprs
      eval' es | [] <- es = return Void
               | otherwise = eval $ head es

eval (Lambda _ args body) = do
  env <- get
  VClosure args body <$> get

eval (FnApp _ fnExpr argExprs) = do
  vals <- mapM eval argExprs
  closure <- eval fnExpr
  env <- get
  let (VInt x) = vals !! 0
  let (VInt y) = vals !! 1

  let (VBool b1) = vals !! 0
  let (VBool b2) = vals !! 1


  case closure of
    (BuiltIn "+") -> lift $ Right $ VInt (x + y)
    (BuiltIn "-") -> lift $ Right $ VInt (x - y)
    (BuiltIn "*") -> lift $ Right $ VInt (x * y)
    (BuiltIn "/") -> lift $ Right $ VInt (x `div` y)
    (BuiltIn "%") -> lift $ Right $ VInt (x `mod` y)

    (BuiltIn "||") -> lift $ Right $ VBool (b1 || b2)
    (BuiltIn "&&") -> lift $ Right $ VBool (b1 && b2)
    (BuiltIn "==") -> lift $ Right $ VBool (vals !! 0 == vals !! 1)
    (BuiltIn "!=") -> lift $ Right $ VBool  (vals !! 0 == vals !! 1)

    (VClosure paramNames bodyExpr capturedEnv) ->
      if length paramNames == length vals then
        let
          pairs = zip paramNames vals
          insert' (Name _ name, v) = Map.insert name v
          env' = foldr insert' capturedEnv pairs
          env'' = Map.union env env'

        in do
          traceM $ show env''
          lift $ evalStateT (eval bodyExpr) env''

      else lift $ Left "Wrong number of params"
    _ -> lift $ Left "Cannot apply this expression"




evalLit :: (Eq a, Show a) => Literal a -> EvalState a
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
      return (name, v)



runEvaluation :: (Eq a, Show a ) => Expr a -> Either String (Value a, Env a)
runEvaluation e = runStateT (eval e) Map.empty




fromMaybe :: a -> Maybe b -> Either a b
fromMaybe _ (Just b) = Right b
fromMaybe a Nothing  = Left a


