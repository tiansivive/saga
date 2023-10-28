

{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}


module Saga.Language.Typechecker.Inference.Type.Expr where

import           Saga.Language.Core.Expr                          (Case (..),
                                                                   Declaration (..),
                                                                   Expr (..),
                                                                   Statement (..))

import qualified Saga.Language.Typechecker.Inference.Inference    as I

import qualified Saga.Language.Typechecker.Kind                   as K
import           Saga.Language.Typechecker.Kind                   (Kind)
import qualified Saga.Language.Typechecker.Type                   as T
import           Saga.Language.Typechecker.Type                   (Scheme (Forall),
                                                                   Type)

import           Control.Monad.RWS
import qualified Data.Map                                         as Map
import qualified Saga.Language.Typechecker.Solver.Constraints     as CST
import           Saga.Language.Typechecker.Solver.Constraints     (Constraint (..))

import           Control.Monad.Except

import           Prelude                                          hiding
                                                                  (lookup)
import           Saga.Language.Typechecker.Errors
import           Saga.Language.Typechecker.Variables              (Classifier,
                                                                   Level (..),
                                                                   VarType)

import           Control.Applicative                              ((<|>))
import           Data.List                                        hiding
                                                                  (lookup)
import qualified Data.Set                                         as Set
import           Saga.Language.Core.Literals                      (Literal (..))
import           Saga.Language.Typechecker.Environment            (CompilerState (..))
import           Saga.Language.Typechecker.Inference.Inference    hiding
                                                                  (lookup)
import           Saga.Language.Typechecker.Protocols              (ProtocolID)
import           Saga.Language.Typechecker.Solver.Substitution    (Subst,
                                                                   Substitutable (..),
                                                                   compose,
                                                                   nullSubst)

import qualified Control.Monad.Writer                             as W
import           Data.Foldable                                    (for_)
import qualified Saga.Language.Typechecker.Evaluation             as E
import qualified Saga.Language.Typechecker.Inference.Type.Pattern as Pat
import qualified Saga.Language.Typechecker.Inference.Type.Shared  as Shared
import           Saga.Language.Typechecker.Lib
import qualified Saga.Language.Typechecker.Qualification          as Q
import           Saga.Language.Typechecker.Qualification          (Qualified (..))
import qualified Saga.Language.Typechecker.TypeExpr               as TE
import           Saga.Language.Typechecker.TypeExpr               (Pattern (..))
import qualified Saga.Language.Typechecker.Variables              as Var
import           Saga.Utils.Operators                             ((|>), (||>))



instance (Generalize Type, Instantiate Type) => Inference Expr where
    infer = infer'
    lookup = lookup'
    fresh = Shared.fresh


infer' :: forall m. (Shared.TypeInference m, Generalize Type, Instantiate Type) => Expr -> m Expr
infer' e = case e of
    e@(Literal literal) -> return $ Typed e (T.Singleton literal)
    Identifier x -> do
      cs :=> t <- lookup' x
      case cs of
        [] -> return $ Typed e t
        _  -> do
          e' <- foldM elaborate (Identifier x) cs
          return $ Typed e' t
      where
        elaborate expr (ty `Q.Implements` protocol) = do
          evidence@(Var.Evidence prtclImpl) <- Shared.fresh E
          emit' $ CST.Impl evidence (CST.Mono ty) protocol
          return $ FnApp expr [Identifier prtclImpl]

    Typed expr ty -> do
        Typed expr' inferred <- infer expr
        evidence <- Shared.fresh E
          -- | Inferred type is generalized as much as possible and unification expects the LHS to be the subtype
        case expr' of
          -- | When expression is a literal, it doesn't get generalized to preserve the literal type
          Literal _ -> emit' $ CST.Equality evidence (CST.Mono inferred) (CST.Mono ty)
          -- | In any other case, we want to unify by instantiating the inferred type to the src type
          _         -> emit' $ CST.Equality evidence (CST.Mono ty) (CST.Mono inferred)
        return e

    Tuple elems -> do
        elems' <- mapM infer elems
        return $ Typed (Tuple elems') (T.Tuple $ fmap extract elems')

    Record pairs -> do
        pairs' <- mapM (mapM infer) pairs
        return $ Typed (Record pairs') (T.Record $ fmap extract <$> pairs')

    List elems -> do
      tvar <- T.Var <$> Shared.fresh U
      elems' <- mapM infer elems
      let tys = fmap extract elems'

      forM_ tys $ \t -> do
        ev <- Shared.fresh E
        emit' $ Equality ev (CST.Mono tvar) (CST.Mono t)

      return $ Typed (List elems') (T.Applied listConstructor tvar)

    Lambda ps@(param : rest) body -> do

        tVar <- Shared.fresh U
        let qt = Forall [] (pure $ T.Var tVar)
        let scoped = local (\e -> e { types = Map.insert param qt $ types e })
        scoped $ do
          o@(Typed _ out') <- infer' out
          let ty = T.Var tVar `T.Arrow` out'
          let expr = Lambda ps o
          return $ Typed expr ty

        where
            out = case rest of
              [] -> body
              _  -> Lambda rest body

    FnApp dot@(Identifier ".") args@[recordExpr, Identifier field] -> do
        fieldType <- T.Var <$> Shared.fresh U
        evidence <- Shared.fresh E
        (Typed _ recordTy) <- infer' recordExpr
        emit' $ CST.Equality evidence (CST.Mono recordTy) (CST.Mono $ T.Record [(field, fieldType)])
        return $ Typed (FnApp dot args) fieldType
    FnApp (Identifier ".") args -> throwError $ Fail $ "Unrecognised expressions for property access:\n\t" ++ show args

    FnApp fn [arg] -> do
        out <- T.Var <$> Shared.fresh U
        fn'@(Typed _ fnTy)   <- infer' fn
        arg'@(Typed _ argTy) <- infer' arg

        inferred <- generalize $ argTy `T.Arrow` out
        evidence <- Shared.fresh E
        emit' $ CST.Equality evidence (CST.Mono fnTy) (CST.Poly inferred)

        return $ Typed (FnApp fn' [arg']) out
    FnApp fn (a : as) -> infer' curried
        where
          partial = FnApp fn [a]
          curried = foldl (\f a -> FnApp f [a]) partial as

    Match scrutinee cases -> do
        scrutinee'@(Typed _ ty) <- infer scrutinee
        cases' <- mapM inferCase cases
        let (tvars, tys) = foldl separate ([], []) cases'
        ty' <- mapM generalize $ case length tys of
              0 -> Nothing
              1 -> Just $ head tys -- | TODO: do we need this anymore? we now collapse unions...
              _ -> Just $ T.Union tys

        for_ ty' $ \t -> do
          ev <- Shared.fresh E
          emit' $ CST.Equality ev (CST.Mono ty) (CST.Poly t)

        forM_ tvars $ \v -> do
          ev <- Shared.fresh E
          emit' $ CST.Equality ev (CST.Mono v) (maybe (CST.Mono ty) CST.Poly ty')

        let out = T.Union $ fmap extractTy cases'
        return $ Typed (Match scrutinee' cases') out

        where

          inferCase (Case pat expr) = do
            (patTy, tyvars) <- Pat.run $ Pat.infer pat
            let (bindings, tvars) = tyvars ||> fmap (\(id, tvar) -> (CST.Binding id tvar, tvar)) |> unzip
            assumps <- asks $ assumptions |> mappend bindings
            let scoped = local (\e -> e { assumptions = assumps })

            (inferred, constraints) <- W.listen $ scoped (infer expr)

            case constraints of
              Left info -> inform' info
              Right cs  -> emit' $ CST.Implication tvars assumps (foldr CST.Conjunction CST.Empty cs)

            return $ TypedCase pat patTy inferred

          extractTy (TypedCase _ _ (Typed _ ty)) = ty
          separate (tvars, tys) caseExpr = case caseExpr of
            TypedCase _ ty@(T.Var _) _ -> (ty:tvars, tys)
            TypedCase _ ty _          -> (tvars, ty:tys)
            _                         -> error $ "Expected to see a typed case: " ++ show caseExpr


    e@(Block stmts) -> walk [] stmts
        where
          walk processed [] = return $ Typed (Block $ reverse processed) returnTy
            where
              returnTy = case head processed of
                (Return (Typed e ty)) -> ty
                _                     -> T.Void
          walk processed (stmt : rest) = case stmt of
            Return expr                   -> do
              typed <- infer expr
              walk (Return typed : processed) []

            d@(Declaration (Type id _ typeExp)) -> do
              env <- ask

              (ty, info) <- E.run typeExp env throwError
              inform' info
              let scoped = local (\e -> e{ types = Map.insert id ty $ types e })
              scoped $ walk (d : processed) rest
            Declaration (Let id (Just typeExp) k expr) -> do
              env <- ask
              (ty, info) <- E.run typeExp env throwError
              let scoped = local (\e -> e{ types = Map.insert id ty $ types e })
              scoped $ do
                (Typed expr' _) <- infer expr
                walk (Declaration (Let id (Just typeExp) k expr') : processed) rest
            Declaration (Let id Nothing k expr) -> do
              tvar <- Shared.fresh U
              let qt = Forall [] (pure $ T.Var tvar)
              let scoped = local (\e -> e{ types = Map.insert id qt $ types e })
              scoped $ do
                (Typed expr' ty) <- infer expr
                ev <- Shared.fresh E
                emit' $ CST.Equality ev (CST.Mono $ T.Var tvar) (CST.Mono ty)
                walk processed rest
            d -> walk (d:processed) rest
    where
      extract (Typed _ ty) = ty


lookup' :: (Shared.TypeInference m, Instantiate Type) => String -> m (Qualified Type)
lookup' x = do
  Saga { types } <- ask

  case Map.lookup x types of
    Just scheme -> inst scheme
    Nothing     -> throwError $ UnboundVariable x

  where
    inst scheme@(Forall [] qt) = return qt
    inst scheme@(Forall tvars _) = Shared.fresh U >>= instantiate scheme . T.Var >>= inst




