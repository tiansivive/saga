{-# LANGUAGE NamedFieldPuns #-}

module Saga.Language.TypeSystem.HindleyMilner.Refinement where

import           Control.Monad.Except
import           Control.Monad.State.Lazy                           (MonadState,
                                                                     State,
                                                                     evalState,
                                                                     evalStateT,
                                                                     replicateM)
import           Control.Monad.Trans.Except                         (ExceptT,
                                                                     runExceptT)

import           Data.Functor                                       ((<&>))

import qualified Data.Map                                           as Map
import qualified Data.Set                                           as Set
import           Debug.Trace                                        (trace,
                                                                     traceM)

import           Saga.Language.TypeSystem.HindleyMilner.Types

import           Control.Monad.Trans.Reader                         (ReaderT (runReaderT))
import           Control.Monad.Trans.State                          (StateT)
import           Data.Bifunctor                                     (first)
import           Prelude                                            hiding (id,
                                                                     lookup)

import           Control.Monad.Identity                             (Identity)

import           Control.Monad.Reader                               (ask, local)
import           Data.List                                          (find)
import           Saga.Language.TypeSystem.HindleyMilner.Environment (CompilerState (Saga, protocols, types),
                                                                     Protocol (Protocol, id),
                                                                     Saga, spec)
import           Saga.Language.TypeSystem.HindleyMilner.Errors      (SagaError (..))
import           Saga.Language.TypeSystem.HindleyMilner.Lib
import           Saga.Parser.ParsingInfo                            hiding
                                                                    (return)


--type Refined a = Saga () (Except RefinementError) a

type Refined = ReaderT CompilerState (Except SagaError)




-- run :: TypeExpr -> Either String Type
-- run tyExpr = do
--   protocols' <- mapM (refine' builtInTypes) protocols
--   refine' (builtInTypes `Map.union` protocols') tyExpr
--   where
--     refine' env t = show `first` runExcept (runReaderT (refine t) env)
--     protocols =  TComposite . TERecord . spec <$> builtInProtocols

runIn' :: CompilerState -> TypeExpr -> Except SagaError Type
runIn' env tyExpr = runReaderT (refine tyExpr) env

runIn :: CompilerState ->  TypeExpr -> Either String Type
runIn env tyExpr = show `first` runExcept (runReaderT (refine tyExpr) env)



lookup :: String -> Refined Type
lookup id = do
    aliases <- ask
    case Map.lookup id (types aliases) of
        Nothing -> throwError $ UndefinedIdentifier id
        Just ty -> refine ty

refine :: TypeExpr -> Refined Type
refine a | trace ("refining: " ++ show a) False = undefined
refine (TAtom ty) = return ty
refine (TIdentifier id) = lookup id
refine (TComposite (TEUnion types)) = TUnion <$> mapM refine types
refine (TComposite (TETuple types)) = TTuple <$> mapM refine types
refine (TComposite (TERecord pairs)) = TRecord <$> mapM (mapM refine) pairs
refine (TComposite (TEArrow in' out')) = TArrow <$> refine in' <*> refine out'


refine (TConditional cond true false) = TUnion <$> mapM refine [true, false]
refine (TClause tyExpr bindings)      = do
    env@(Saga {types}) <- ask
    te <- foldM constrainImpls tyExpr bindings

    let scoped = local (\e -> e{ types = updated types })
    scoped $ refine te
    scoped $ refine tyExpr

      where
        updated tys = foldl extend tys bindings `Map.union` tys
        extend env binding = case binding of
          Bind id tyExpr' -> Map.insert id tyExpr' env
          _               -> env

        constrainImpls :: TypeExpr -> Binding TypeExpr -> Refined TypeExpr
        constrainImpls tyExpr' binding = case binding of
          ImplBind (TQualified (cs :=> tyExpr'')) protocol -> do
            ty <- refine tyExpr''
            return $ TQualified $ ty `Implements` protocol : cs :=> tyExpr''
          ImplBind tyExpr'' protocol -> do
            ty <- refine tyExpr''
            return $ TQualified $ [ty `Implements` protocol] :=> tyExpr''
          _ -> return tyExpr'


refine (TLambda params body) = do
  Saga {types} <- ask
  return $ TClosure params body types
refine (TFnApp fnExpr argExprs) = do
  fn <- refine fnExpr
  args <- mapM refine argExprs

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
        apply tyExpr params [] = do
          Saga {types} <- ask
          return $ TClosure params tyExpr types
        apply tyExpr (p:params) (a: args) = do
            let scoped = local (\e -> e{ types = Map.insert p (TAtom a) (types e) })
            scoped $ apply tyExpr params args

        apply _ [] (a: args) = throwError $ TooManyArguments fnExpr argExprs

refine (TQualified (cs :=> typeExpr)) = refine typeExpr
refine (TImplementation prtcl tyExpr) = do
  Saga {protocols} <- ask
  case find (\(Protocol { id }) -> id == prtcl) protocols of
    Nothing                -> error $ "Could not find Protocol: " ++ prtcl
    Just (Protocol {spec}) -> refine $ TFnApp spec [tyExpr]








