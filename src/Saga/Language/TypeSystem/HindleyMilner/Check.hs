{-# LANGUAGE GADTs #-}
module Saga.Language.TypeSystem.HindleyMilner.Check where
import           Control.Monad.Except                               (Except,
                                                                     MonadError (throwError),
                                                                     runExcept)
import           Control.Monad.Reader                               (ReaderT (runReaderT))
import           Control.Monad.RWS                                  (RWST (runRWST),
                                                                     execRWST)
import qualified Data.Map                                           as Map
import           Saga.Language.TypeSystem.HindleyMilner.Constraints (HasKind (kind),
                                                                     ProtocolEnv,
                                                                     Solve,
                                                                     Subst,
                                                                     Substitutable (apply, ftv),
                                                                     compose,
                                                                     isSubtype,
                                                                     nullSubst,
                                                                     runSolve,
                                                                     unify)

import           Control.Monad.Trans.RWS                            (evalRWST)
import           Data.Bifunctor                                     (first)
import           Debug.Trace                                        (trace,
                                                                     traceM)
import           Saga.Language.TypeSystem.HindleyMilner.Environment (IConstraint,
                                                                     Infer,
                                                                     InferenceEnv,
                                                                     InferenceError (..),
                                                                     InferenceState,
                                                                     Scheme)

import           Saga.Language.TypeSystem.HindleyMilner.Inference   (closeOver,
                                                                     infer,
                                                                     runInfer)
import           Saga.Language.TypeSystem.HindleyMilner.Types       (Kind (KType),
                                                                     PrimitiveType (..),
                                                                     Type (..),
                                                                     Tyvar (Tyvar))

import           Saga.Language.Core.Syntax
import           Saga.Language.TypeSystem.HindleyMilner.Shared

import           Control.Monad                                      (zipWithM)
import           Data.Either
import qualified Data.Set                                           as Set
import           Saga.Language.Core.Literals                        (Literal (..))
import qualified Saga.Language.TypeSystem.HindleyMilner.Refinement  as Refine





type Check = ReaderT ProtocolEnv (Except InferenceError)


check :: Expr -> Type -> Either String ((Subst, Bool), InferenceState, [IConstraint])
check expr ty = do
    tyExpr <- show `first` runInfer (infer expr)
    inferred <- Refine.run tyExpr

    show `first` runExcept (runRWST (unification inferred ty) empty initState)

    where
      tvars = ftv ty
      assignedSpecificType sub tv = case Map.lookup tv sub of
        Nothing       -> False
        Just (TVar _) -> False
        _             -> True

      unification inferred' ty'= do
        sub <- inferred' `unify` ty'
        traceM $ "\nFTV type:\t" ++ show tvars
        traceM $ "Assigned?:\t" ++ show (assignedSpecificType sub <$> Set.toList tvars)
        let bool = not $ any (assignedSpecificType sub) tvars
        return (sub, bool)


