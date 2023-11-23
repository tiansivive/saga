{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE FlexibleInstances #-}


module Saga.Language.Evaluation where


import           Data.ByteString.Lazy.Char8  (ByteString)
import qualified Data.ByteString.Lazy.Char8  as BS
import qualified Data.Map                    as Map

import           Control.Monad.Except        (ExceptT, MonadError (throwError),
                                              runExcept)
import           Control.Monad.Reader        (MonadReader (ask),
                                              ReaderT (runReaderT), asks)
import           Control.Monad.State.Lazy
import           Control.Monad.Trans.Except  (Except)
import           Data.List                   (find, findIndex)
import           Data.Maybe                  (fromJust, fromMaybe)
import           Debug.Trace                 (traceM)

import           Saga.Language.Core.Expr     (Binding (Bind), Declaration (..),
                                              Expr (..))
import           Saga.Language.Core.Literals (Literal (..))
import           Saga.Utils.Common           hiding (fromMaybe)



builtInEnv :: [String]
builtInEnv = ["+", "-","*", "^", "/", "%", "<", ">", "<=", ">=", "==", "!=", "||", "&&"]

data Value
  = VNum Float
  | VBool Bool
  | VString String
  | VList [Value ]
  | VTuple [Value ]
  | VRecord [(String, Value )]
  | VClosure [String] Expr Env
  | BuiltIn String
  | Void
  deriving (Show, Eq)

type Env  = Map.Map String Value

type Evaluated = ReaderT Env (Except String)

instance MonadFail (Either String) where
  fail err = Left $ "Something went wrong:" <> err


unidentified :: String -> String
unidentified id =  "Undefined identifier \"" <> id <> "\""

eval :: Expr -> Evaluated Value
eval (Literal l) = evalLiteral l

eval (Identifier name) =
    if name `elem` builtInEnv then
      return $ BuiltIn name
    else do
      env <- ask
      case Map.lookup name env of
        Just val -> return val
        Nothing  -> throwError $ unidentified name

-- eval (Clause e bindings ) = do
--   bindings' <- mapM evalBinding bindings
--   eval e `scoped` Map.union (Map.fromList bindings')

-- eval (IfElse cond onTrue onFalse) = do
--   val <- eval cond
--   case val of
--     (VBool True) -> eval onTrue
--     (VBool False) -> eval onFalse
--     _ -> throwError "Could not evaluate non boolean expression as a if condition"

-- eval (Block exprs) = do
--   mapM_ eval exprs'
--   eval' rest
--     where
--       returnStmt e | (Return _) <- e = True
--                    | otherwise         = False
--       (exprs', rest) = break returnStmt exprs
--       eval' es | [] <- es = return Void
--                | otherwise = eval $ head es

eval (Lambda args body) = asks (VClosure args body)

eval (FnApp fnExpr argExprs) = do
  vals <- mapM eval argExprs
  closure <- eval fnExpr
  env <- ask
  let (VNum x) = head vals
  let (VNum y) = vals !! 1

  let (VBool b1) = head vals
  let (VBool b2) = vals !! 1



  case closure of
    (BuiltIn "+") -> return $ VNum (x + y)
    (BuiltIn "-") -> return $ VNum (x - y)
    (BuiltIn "*") -> return $ VNum (x * y)
    (BuiltIn "/") -> return $ VNum (x / y)
    --(BuiltIn "%") -> return $ VNum (x `mod` y)

    (BuiltIn "||") -> return $ VBool (b1 || b2)
    (BuiltIn "&&") -> return $ VBool (b1 && b2)
    (BuiltIn "==") -> return $ VBool (head vals == vals !! 1)
    (BuiltIn "!=") -> return $ VBool  (head vals == vals !! 1)

    (BuiltIn "<")  -> return $ VBool (x < y)
    (BuiltIn "<=") -> return $ VBool (x <= y)
    (BuiltIn ">")  -> return $ VBool (x > y)
    (BuiltIn ">=") -> return $ VBool  (x >= y)

    (VClosure paramNames bodyExpr capturedEnv) ->
      if length paramNames == length vals then
        let
          pairs = zip paramNames vals
          insert' (name, v) = Map.insert name v
          env' = foldr insert' capturedEnv pairs
        in
          eval bodyExpr `scoped` Map.union env'
          --lift $ evalStateT (eval bodyExpr) env''

      else throwError "Wrong number of params"
    _ -> throwError "Cannot apply this expression"




evalLiteral :: Literal -> Evaluated Value
evalLiteral (LInt int)    = return $ VNum int
evalLiteral (LBool bool)  = return $ VBool bool
evalLiteral (LString str) = return $ VString str
-- evalLiteral (LList _ list) = do
--   vals <- mapM eval list
--   return $ VList vals
-- evalLiteral (LTuple _ tuple)  = do
--   vals <- mapM eval tuple
--   return $ VTuple vals
-- evalLiteral (LRecord _ record) = do
--   vals <- mapM evalPair record
--   return $ VRecord vals
--   where
--     evalPair (Name _ name, e) = do
--       v <- eval e
--       return (name, v)


evalBinding :: Binding Expr -> Evaluated (String, Value)
evalBinding (Bind id expr) = sequence (id, eval expr)

evalDeclaration :: Declaration -> Evaluated (String, Value)
evalDeclaration (Let name _ _ expr) = sequence (name, eval expr)
evalDeclaration _                   = throwError "Tried to evaluate non let declaration"

runEvaluated :: Expr -> Except String Value
runEvaluated e = runReaderT (eval e) Map.empty

run :: Maybe Env -> Evaluated a -> Either String a
run env evaluation = runExcept $ runReaderT evaluation $ fromMaybe Map.empty env


