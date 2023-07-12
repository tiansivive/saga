

module Saga.AST.TypeSystem.HindleyMilner.Refinement where

import           Control.Monad.Except
import           Control.Monad.State.Lazy                (MonadState, State,
                                                          evalState, evalStateT,
                                                          replicateM)
import           Control.Monad.Trans.Except              (ExceptT, runExceptT)

import           Data.Functor                            ((<&>))

import qualified Data.Map                                as Map
import qualified Data.Set                                as Set
import           Debug.Trace                             (trace, traceM)

import           Saga.AST.TypeSystem.HindleyMilner.Types

import           Control.Monad.Trans.Reader              (ReaderT, ask, local)
import           Control.Monad.Trans.State               (StateT)
import           Prelude                                 hiding (lookup)
import           Saga.Parser.ParsingInfo


type RefinementEnv = Map.Map String Type


type Refined = ReaderT RefinementEnv (Except RefinementError)

data RefinementError
    = UnexpectedType String
    | UnboundIdentifier String



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
refine (TParens tyExp) = refine tyExp
refine (TETuple tyExps) = mapM refine tyExps <&> TTuple
refine (TERecord pairs) = mapM (mapM refine) pairs <&> TRecord
refine (TEArrow input output) = do
  in' <- refine input
  out' <- refine output
  return $ in' `TArrow` out'
refine (TConditional cond true false) = refine true -- assumes same return type, will change with union types
refine (TIdentifier id) = lookup id
refine (TFnApp fnExpr argExprs) = do
  args <- mapM refine argExprs
  constructor <- refine fnExpr

  traceM $ "Fn Expression: " <> show constructor
  traceM $ "Fn Args: " <> show args
  case (constructor, argExprs) of
    (TParametric param body, args) -> refine' param body args
    (TVar t, args) -> do
        ty <- lookup t
        case ty of
            TParametric param body -> refine' param body args
            _ -> throwError $ UnexpectedType "Cannot apply this type expression"
    _ -> throwError $ UnexpectedType "Cannot apply this type expression"

    where
        refine' param body (arg:as) = do
            ty <- refine arg
            let scoped = local $ Map.insert param ty
            scoped $ refine $ case as of
                [] -> body
                _  -> TFnApp body as

refine (TLambda [param] body) = return $ TParametric param body
refine (TLambda (p : ps) body) = return $ TParametric p $ TLambda ps body
