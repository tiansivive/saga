{-# LANGUAGE LambdaCase #-}


module Saga.Language.TypeSystem.HindleyMilner.Refinement where

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

import           Saga.Language.TypeSystem.HindleyMilner.Types

import           Control.Monad.Trans.Reader                    (ReaderT (runReaderT),
                                                                ask, local)
import           Control.Monad.Trans.State                     (StateT)
import           Data.Bifunctor                                (first)
import           Prelude                                       hiding (lookup)
import           Saga.Language.TypeSystem.HindleyMilner.Environment (scoped)
import           Saga.Parser.ParsingInfo                       hiding (return)


type RefinementEnv = Map.Map String Type


type Refined = ReaderT RefinementEnv (Except RefinementError)

data RefinementError
    = UnexpectedType String
    | UnboundIdentifier String String
    | TooManyArguments TypeExpr [TypeExpr]
  deriving (Show)




run :: TypeExpr -> Either String Type
run tyExpr = show `first` runExcept (runReaderT (refine tyExpr) builtInTypes)


builtInTypes :: RefinementEnv
builtInTypes = Map.fromList
  [ ("Int", TPrimitive TInt)
  , ("Bool", TPrimitive TBool)
  , ("String", TPrimitive TString)
  , ("List", TData (Tycon "List" (KArrow KType KType)))
  , ("Function", TData (Tycon "Function" (KArrow KType (KArrow KType KType))))
  ]



lookup :: String -> Refined Type
lookup id = do
    aliases <- ask
    case Map.lookup id aliases of
        Nothing -> throwError $ UnboundIdentifier id ""
        Just ty -> return ty

refine :: TypeExpr -> Refined Type
refine a | trace ("refining: " ++ show a) False = undefined
-- refine (Type ty)                      = return ty
-- refine (TClause _ tyExp)              = refine tyExp
-- refine (TBlock [])                    = return TUnit
-- refine (TBlock tyExps)                = refine $ last tyExps
-- refine (TReturn tyExp)                = refine tyExp
refine (TTerm term) = return $ TLiteral term
refine (TIdentifier id) = lookup id
refine (TParens tyExp) = refine tyExp
refine (TETuple tyExps) = mapM refine tyExps <&> TTuple
refine (TERecord pairs) = mapM (mapM refine) pairs <&> TRecord
refine (TEArrow input output) = do
  in' <- refine input
  out' <- refine output
  return $ in' `TArrow` out'
refine (TConditional cond true false) = refine $ TEUnion [true, false] -- assumes same return type, will change with union types
refine (TEUnion tyExprs) = TUnion <$> mapM refine tyExprs
refine (TClause tyExpr bindings) = do
    env <- ask
    env' <- foldM extend env bindings
    env'' <- foldM constrainImpls env' bindings

    let scoped = local (env'' `Map.union`)

    scoped $ refine tyExpr

      where
        extend env binding = case binding of
          Bind id tyExpr' -> refine tyExpr' <&> Map.insert id <*> return env
          _               -> return env

        constrainImpls :: RefinementEnv -> Binding TypeExpr -> Refined RefinementEnv
        constrainImpls env binding = case binding of
          ImplBind id protocol -> Map.alterF (\case
              Just ty' -> return $ Just $ TQualified ([ty' `Implements` protocol] :=> ty')
              Nothing -> throwError $ UnboundIdentifier id $ "When trying to implement protocol: " ++ protocol
            ) id env
          _ -> return env


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








