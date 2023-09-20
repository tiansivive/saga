{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}
module Saga.Language.TypeSystem.HindleyMilner.Check where
import           Control.Monad.Except                               (Except,
                                                                     MonadError (throwError),
                                                                     MonadTrans (lift),
                                                                     foldM,
                                                                     runExcept)
import           Control.Monad.Reader                               (ReaderT (runReaderT))
import           Control.Monad.RWS                                  (RWST (runRWST),
                                                                     evalRWS,
                                                                     execRWST,
                                                                     runRWS)
import qualified Data.Map                                           as Map
import           Saga.Language.TypeSystem.HindleyMilner.Constraints (HasKind (kind),
                                                                     Solve,
                                                                     Subst,
                                                                     Substitutable (apply, ftv),
                                                                     compose,
                                                                     isSubtype,
                                                                     nullSubst,
                                                                     runSolve,
                                                                     unify)

import           Control.Monad.Trans.RWS                            (evalRWST,
                                                                     execRWS)
import           Data.Bifunctor                                     (first)
import           Debug.Trace                                        (trace,
                                                                     traceM)
import           Saga.Language.TypeSystem.HindleyMilner.Environment (Accumulator,
                                                                     CompilerState (types),
                                                                     IConstraint,
                                                                     InferenceEnv,
                                                                     Saga,
                                                                     Scheme,
                                                                     Tell (Error),
                                                                     log)

import           Saga.Language.TypeSystem.HindleyMilner.Inference   (Infer,
                                                                     InferState,
                                                                     closeOver,
                                                                     infer,
                                                                     initState,
                                                                     runInfer)

import           Saga.Language.TypeSystem.HindleyMilner.Types       (Kind (KType),
                                                                     PrimitiveType (..),
                                                                     Type (..),
                                                                     TypeExpr,
                                                                     Tyvar (Tyvar))

import           Saga.Language.Core.Syntax
import           Saga.Language.TypeSystem.HindleyMilner.Shared

import           Control.Monad                                      (zipWithM)
import           Control.Monad.Identity                             (Identity)
import           Control.Monad.Reader                               (ask)
import           Control.Monad.Trans.RWS                            (get,
                                                                     modify)
import           Data.Either
import           Data.Functor                                       ((<&>))
import qualified Data.Set                                           as Set
import           Prelude                                            hiding (log)
import           Saga.Language.Core.Literals                        (Literal (..))
import           Saga.Language.TypeSystem.HindleyMilner.Errors      (SagaError)
import           Saga.Language.TypeSystem.HindleyMilner.Lib         (defaultEnv,
                                                                     startWriter)
import qualified Saga.Language.TypeSystem.HindleyMilner.Refinement  as Refine





type Check = ReaderT CompilerState (Except SagaError)
type Result = ((Subst, Bool), InferState, [IConstraint])



run :: Check Result -> Either String Result
run check = show `first` runExcept (runReaderT check defaultEnv)

check :: Expr -> TypeExpr -> Check Result
check expr tyExpr = do
    env <- ask
    ty <- Refine.refine tyExpr
    inferredTyExpr <- lift $ runInfer env (infer expr)
    inferred <- Refine.refine inferredTyExpr
    lift $ runRWST (unification inferred ty) env initState

    where

      assignedSpecificType sub tv = case Map.lookup tv sub of
        Nothing       -> False
        Just (TVar _) -> False
        _             -> True

      unification :: Type -> Type -> Infer (Subst, Bool)
      unification inferred' ty'= do
        sub <- inferred' `unify` ty'
        -- traceM $ "\nFTV type:\t" ++ show (ftv ty')
        -- traceM $ "Assigned?:\t" ++ show (assignedSpecificType sub <$> Set.toList (ftv ty'))
        let bool = not $ any (assignedSpecificType sub) (ftv ty')
        return (sub, bool)



checkScript :: Script -> ([Maybe Result], CompilerState, Accumulator)
checkScript (Script decs) = runRWS (mapM saga decs) () defaultEnv
  where

    -- saga :: Declaration -> Saga Identity (Maybe Result)
    -- saga dec = do
    --   state <- get
    --   case runExcept (runReaderT (check' dec) state) of
    --     Left err -> do
    --       log $ Error err
    --       return Nothing
    --     Right result -> return $ Just result
    check' e t st = runExcept (runReaderT (check e t) st)
    saga :: Declaration -> Saga Identity (Maybe Result)
    saga (Let _ (Just tyExpr) _ expr) = do
      state <- get
      case check' expr tyExpr state of
        Left err -> do
          log $ Error err
          return Nothing
        Right result -> return $ Just result

    saga (Let id Nothing _ expr) = do
      state <- get
      case checkInferred expr state of
        Left err -> do
          log $ Error err
          return Nothing
        Right (result, inferred) -> do
          modify $ \s -> s{ types = Map.insert id inferred $ types s}
          return $ Just result

      where
        checkInferred expr st = do
          inferred <- runExcept $ runInfer st (infer expr)
          result <- check' expr inferred st
          return (result, inferred)

    saga (Type id _ tyExpr) = do
      modify $ \s -> s{ types = Map.insert id tyExpr $ types s}
      return Nothing

    saga _ = return Nothing
    -- check' = \case
    --   (Let _ (Just tyExpr) _ expr ) -> check expr tyExpr
    --   (Let _ Nothing _ expr ) -> do
    --     inferredTyExpr <- show `first` runInfer (infer expr)
    --     check expr inferredTyExpr




