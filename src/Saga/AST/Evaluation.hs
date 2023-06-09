{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE FlexibleInstances #-}


module Saga.AST.Evaluation where

import           Saga.AST.Scripts           (Declaration (..))
import           Saga.AST.Syntax            (Expr (..), Name (..), Term (..))

import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map                   as Map

import           Control.Monad.State.Lazy
import           Data.List                  (find, findIndex)
import           Debug.Trace                (traceM)
import           Saga.Utils.Utils



builtInEnv :: [String]
builtInEnv = ["+", "-","*", "^", "/", "%", "<", ">", "<=", ">=", "==", "!=", "||", "&&"]

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

instance MonadFail (Either String) where
  fail err = Left $ "Something went wrong:" <> err


unidentified :: String -> String
unidentified id =  "Undefined identifier \"" <> id <> "\""

eval :: (Eq a, Show a) => Expr a -> EvalState a
eval (Term l) = evalTerm l
eval (Parens _ e) = eval e
eval (Return _ e) = eval e

eval (FieldAccess _  expr path) = do
    VRecord pairs <- eval expr
    eval' pairs path
      where
        find' :: String -> [(String, Value a)] -> EvalState a
        find' id pairs
          | Just (_ , val) <- find (\(name, _) -> name == id) pairs = return val
          | otherwise = lift $ Left $ "Could not find property " <> id

        eval' pairs [Name _ id] = find' id pairs
        eval' pairs ((Name _ id):rest) = do
          VRecord pairs' <- find' id pairs
          eval' pairs' rest


eval (Identifier (Name _ name)) =
    if name `elem` builtInEnv
      then return $ BuiltIn name
      else  do
        env <- get
        let val = Map.lookup name env
        lift $ fromMaybe (unidentified name) val

eval (Assign (Name _ name) e )  = do
      val <- eval e
      modify $ Map.insert name val
      return val

eval (Clause _ assignments e) =
  let
    eval' = do
      mapM_ eval assignments
      eval e
  in do
      env <- get
      lift $ evalStateT eval' env

eval (IfElse _ cond onTrue onFalse) = do
  val <- eval cond
  case val of
    (VBool True) -> eval onTrue
    (VBool False) -> eval onFalse
    _ -> lift $ Left "Could not evaluate non boolean expression as a if condition"

eval (Block _ exprs) = do
  mapM_ eval exprs'
  eval' rest
    where
      returnStmt e | (Return _ _) <- e = True
                   | otherwise         = False
      (exprs', rest) = break returnStmt exprs
      eval' es | [] <- es = return Void
               | otherwise = eval $ head es

eval (Lambda _ args body) = VClosure args body <$> get

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

    (BuiltIn "<") -> lift $ Right $ VBool (x < y)
    (BuiltIn "<=") -> lift $ Right $ VBool (x <= y)
    (BuiltIn ">") -> lift $ Right $ VBool (x > y)
    (BuiltIn ">=") -> lift $ Right $ VBool  (x >= y)

    (VClosure paramNames bodyExpr capturedEnv) ->
      if length paramNames == length vals then
        let
          pairs = zip paramNames vals
          insert' (Name _ name, v) = Map.insert name v
          env' = foldr insert' capturedEnv pairs
          env'' = Map.union env' env -- left side overrides any duplicates

        in
          lift $ evalStateT (eval bodyExpr) env''

      else lift $ Left "Wrong number of params"
    _ -> lift $ Left "Cannot apply this expression"




evalTerm :: (Eq a, Show a) => Term a -> EvalState a
evalTerm (LInt _ int)    = return $ VInt int
evalTerm (LBool _ bool)  = return $ VBool bool
evalTerm (LString _ str) = return $ VString $ BS.unpack str
evalTerm (LList _ list) = do
  vals <- mapM eval list
  return $ VList vals
evalTerm (LTuple _ tuple)  = do
  vals <- mapM eval tuple
  return $ VTuple vals
evalTerm (LRecord _ record) = do
  vals <- mapM evalPair record
  return $ VRecord vals
  where
    evalPair (Name _ name, e) = do
      v <- eval e
      return (name, v)


evalDeclaration :: (Eq a, Show a) => Declaration a -> EvalState a
evalDeclaration (Let name _ _ expr) = eval $ Assign name expr
evalDeclaration _                   = return Void

runEvaluation :: (Eq a, Show a ) => Expr a -> Either String (Value a, Env a)
runEvaluation e = runStateT (eval e) Map.empty





