

module Saga.AST.TypeSystem.HindleyMilner.Refinement where

import           Control.Monad.Except
import           Control.Monad.State.Lazy                      (MonadState,
                                                                State,
                                                                evalState,
                                                                evalStateT,
                                                                replicateM)
import           Control.Monad.Trans.Except                    (ExceptT,
                                                                runExceptT)

import           Data.Functor                                  ((<&>))

import qualified Data.Map                                      as Map
import qualified Data.Set                                      as Set
import           Debug.Trace                                   (trace, traceM)

import           Saga.AST.TypeSystem.HindleyMilner.Types

import           Control.Monad.Trans.Reader                    (ReaderT, ask,
                                                                local)
import           Control.Monad.Trans.State                     (StateT)
import           Prelude                                       hiding (lookup)
import           Saga.AST.TypeSystem.HindleyMilner.Environment (scoped)
import           Saga.Parser.ParsingInfo


type RefinementEnv = Map.Map String Type


type Refined = ReaderT RefinementEnv (Except RefinementError)

data RefinementError
    = UnexpectedType String
    | UnboundIdentifier String
    | TooManyArguments TypeExpr [TypeExpr]



lookup :: String -> Refined Type
lookup id = do
    aliases <- ask
    case Map.lookup id aliases of
        Nothing -> throwError $ UnboundIdentifier id
        Just ty -> return ty

refine :: TypeExpr -> Refined Type
-- refine a | trace ("refining: " ++ show a) False = undefined
-- refine (Type ty)                      = return ty
-- refine (TClause _ tyExp)              = refine tyExp
-- refine (TBlock [])                    = return TUnit
-- refine (TBlock tyExps)                = refine $ last tyExps
-- refine (TReturn tyExp)                = refine tyExp
refine (TIdentifier id) = lookup id
refine (TParens tyExp) = refine tyExp
refine (TETuple tyExps) = mapM refine tyExps <&> TTuple
refine (TERecord pairs) = mapM (mapM refine) pairs <&> TRecord
refine (TEArrow input output) = do
  in' <- refine input
  out' <- refine output
  return $ in' `TArrow` out'
refine (TConditional cond true false) = refine $ TEUnion true false -- assumes same return type, will change with union types
refine (TEUnion true false) = do
    true' <- refine true
    false' <- refine false
    return $ true' `TUnion` false'

refine (TLambda params body) = ask <&> TClosure params body
refine (TFnApp fnExpr argExprs) = do
  fn <- refine fnExpr
  args <- mapM refine argExprs

--   traceM $ "Fn Expression: " <> show fn
--   traceM $ "Fn Args: " <> show args
  case (fn, args) of
    (t@(TData tycon), args) -> return $ foldl TApplied t args
    (TClosure params closure env, args) -> apply closure params args

    (TVar (Tyvar t _), args) -> do
        ty <- lookup t
        case ty of
            t@(TData tycon) -> return $ foldl TApplied t args
            TClosure params closure env -> apply closure params args
            _ -> throwError $ UnexpectedType "Cannot apply this type expression"
    _ -> throwError $ UnexpectedType "Cannot apply this type expression"

    where

        apply :: TypeExpr -> [String] -> [Type] -> Refined Type
        apply tyExpr [] [] = refine tyExpr
        apply tyExpr params [] = ask <&> TClosure params tyExpr
        apply tyExpr (p:params) (a: args) = do
            let scoped = local $ Map.insert p a
            scoped $ apply tyExpr params args

        apply _ [] (a: args) = throwError $ TooManyArguments fnExpr argExprs








