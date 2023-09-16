


module Saga.Language.TypeSystem.HindleyMilner.Refinement where

import           Control.Monad.Except
import           Control.Monad.State.Lazy                     (MonadState,
                                                               State, evalState,
                                                               evalStateT,
                                                               replicateM)
import           Control.Monad.Trans.Except                   (ExceptT,
                                                               runExceptT)

import           Data.Functor                                 ((<&>))

import qualified Data.Map                                     as Map
import qualified Data.Set                                     as Set
import           Debug.Trace                                  (trace, traceM)

import           Saga.Language.TypeSystem.HindleyMilner.Types

import           Control.Monad.Trans.Reader                   (ReaderT (runReaderT),
                                                               ask, local)
import           Control.Monad.Trans.State                    (StateT)
import           Data.Bifunctor                               (first)
import           Prelude                                      hiding (lookup)

import           Saga.Language.TypeSystem.HindleyMilner.Lib
import           Saga.Parser.ParsingInfo                      hiding (return)

type RefinementEnv = Map.Map String Type
type Refined = ReaderT RefinementEnv (Except RefinementError)

data RefinementError
    = UnexpectedType String
    | UnboundIdentifier String String
    | TooManyArguments TypeExpr [TypeExpr]
  deriving (Show)


run :: TypeExpr -> Either String Type
run tyExpr = show `first` runExcept (runReaderT (refine tyExpr) builtInTypes)

runIn :: RefinementEnv ->  TypeExpr -> Either String Type
runIn env tyExpr = show `first` runExcept (runReaderT (refine tyExpr) (builtInTypes `Map.union` env))



lookup :: String -> Refined Type
lookup id = do
    aliases <- ask
    case Map.lookup id aliases of
        Nothing -> throwError $ UnboundIdentifier id ""
        Just ty -> return ty

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
    env <- ask
    env' <- foldM extend env bindings
    te <- foldM constrainImpls tyExpr bindings

    let scoped = local (env' `Map.union`)
    scoped $ refine te

    scoped $ refine tyExpr

      where
        extend env binding = case binding of
          Bind id tyExpr' -> do
            ty' <- refine tyExpr'
            return $ Map.insert id ty' env
          _               -> return env

        constrainImpls :: TypeExpr -> Binding TypeExpr -> Refined TypeExpr
        constrainImpls tyExpr' binding = case binding of
          ImplBind (TQualified (cs :=> tyExpr'')) protocol -> do
            ty <- refine tyExpr''
            return $ TQualified $ ty `Implements` protocol : cs :=> tyExpr''
          ImplBind tyExpr'' protocol -> do
            ty <- refine tyExpr''
            return $ TQualified $ [ty `Implements` protocol] :=> tyExpr''
          _ -> return tyExpr'


refine (TLambda params body) = ask <&> TClosure params body
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
        apply tyExpr params [] = ask <&> TClosure params tyExpr
        apply tyExpr (p:params) (a: args) = do
            let scoped = local $ Map.insert p a
            scoped $ apply tyExpr params args

        apply _ [] (a: args) = throwError $ TooManyArguments fnExpr argExprs

refine (TQualified (cs :=> typeExpr)) = refine typeExpr









