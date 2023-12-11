

{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}


module Saga.Language.Typechecker.Inference.Type.Expr where

import           Saga.Language.Core.Expr                                 (Case (..),
                                                                          Declaration (..),
                                                                          Expr (..),
                                                                          Statement (..))

import qualified Saga.Language.Typechecker.Inference.Inference           as I

import qualified Saga.Language.Typechecker.Kind                          as K
import           Saga.Language.Typechecker.Kind                          (Kind)
import qualified Saga.Language.Typechecker.Type                          as T
import           Saga.Language.Typechecker.Type                          (Scheme (Forall),
                                                                          Type)

import           Control.Monad.RWS
import qualified Data.Map                                                as Map
import qualified Saga.Language.Typechecker.Solver.Constraints            as CST
import           Saga.Language.Typechecker.Solver.Constraints            (Constraint (..))

import           Control.Monad.Except

import           Prelude                                                 hiding
                                                                         (lookup)
import           Saga.Language.Typechecker.Errors
import           Saga.Language.Typechecker.Variables                     (PolymorphicVar,
                                                                          VarType)

import           Control.Applicative                                     ((<|>))
import           Data.List                                               hiding
                                                                         (lookup)
import qualified Data.Set                                                as Set
import           Saga.Language.Core.Literals                             (Literal (..))
import           Saga.Language.Typechecker.Environment                   (CompilerState (..))
import           Saga.Language.Typechecker.Inference.Inference           hiding
                                                                         (lookup)
import           Saga.Language.Typechecker.Protocols                     (ProtocolID)
import           Saga.Language.Typechecker.Solver.Substitution           (Subst,
                                                                          Substitutable (..),
                                                                          compose,
                                                                          mkSubst,
                                                                          nullSubst)

import qualified Control.Monad.Writer                                    as W
import           Data.Foldable                                           (for_)
import qualified Effectful                                               as Eff
import qualified Effectful.Error.Static                                  as Eff
import qualified Effectful.Reader.Static                                 as Eff
import qualified Effectful.Writer.Static.Local                           as Eff
import qualified Saga.Language.Typechecker.Evaluation                    as E
import qualified Saga.Language.Typechecker.Inference.Type.Pattern        as Pat
import qualified Saga.Language.Typechecker.Inference.Type.Shared         as Shared
import           Saga.Language.Typechecker.Lib
import qualified Saga.Language.Typechecker.Qualification                 as Q
import           Saga.Language.Typechecker.Qualification                 (Given (..),
                                                                          Qualified (..))

import qualified Saga.Language.Typechecker.TypeExpr                      as TE
import           Saga.Language.Typechecker.TypeExpr                      (Pattern (..))
import qualified Saga.Language.Typechecker.Variables                     as Var
import           Saga.Utils.Operators                                    ((|>),
                                                                          (||>))

import           Data.Functor                                            ((<&>))
import           Data.Map                                                (Map)
import           Data.Maybe                                              (catMaybes)
import           Debug.Pretty.Simple                                     (pTrace,
                                                                          pTraceM)
import           Effectful                                               (Eff,
                                                                          (:>))
import qualified Effectful.Fail                                          as Eff
import qualified Effectful.State.Static.Local                            as Eff
import           Saga.Language.Typechecker.Inference.Type.Generalization
import           Saga.Language.Typechecker.Inference.Type.Instantiation

instance (Generalize Type, Instantiate Type) => Inference Expr where
    infer = infer'
    lookup = lookup'
    fresh = Shared.fresh

type Bindings = Map (PolymorphicVar Type) (Qualified Type)

infer' :: Shared.TypeInference es => Expr -> Eff es Expr
infer' e = case e of
    e@(Literal literal) -> return $ Typed e (T.Singleton literal)
    Identifier x -> do
      ty@(_ :=> t) <- lookup' x
      (skolems, assumptions, cs) <- contextualize ty

      case cs of
        [] -> return $ Typed e t
        _  -> do
          (e', cs') <- foldM elaborate (Identifier x, []) cs
          Eff.tell $ CST.Implication skolems assumptions (foldl CST.Conjunction CST.Empty cs')
          return $ Typed e' t

      where

        contextualize ty@(bs :| cs :=> t) = do
          pTraceM "\nContextualizing:"
          pTraceM $ show ty
          (cs', sub) <- Eff.runState @(Subst Type) nullSubst . Eff.runReader bs $ concat <$> forM (locals t) collect
          l <- Eff.gets level
          assumptions <- fmap CST.Assume <$> forM (Map.toList sub) (\(local, t) -> do
            ev <- Shared.fresh E
            return $ CST.Equality ev (CST.Variable (CST.Level l) local) (CST.Mono t))

          return (locals t, assumptions, cs <> cs')


        elaborate (expr, cs) (ty `Q.Implements` protocol) = do
          evidence@(CST.Evidence prtclImpl) <- Shared.fresh E
          let constraint = CST.Impl evidence (CST.Mono ty) protocol
          let expr' = FnApp expr [Identifier prtclImpl]
          return (expr', constraint : cs)
        elaborate (expr, cs) (Q.Refinement binds liquid ty) = do
          let constraint = CST.Refined (fmap CST.Mono binds) (CST.Mono ty) liquid
          return (expr, constraint : cs)


        locals :: Type -> [PolymorphicVar Type]
        locals t = [ v | v@(T.Local {}) <- Set.toList $ ftv t ]

        collect :: (Eff.Reader Bindings :> es, Eff.State (Subst Type) :> es) => PolymorphicVar Type -> Eff es [Q.Constraint Type]
        collect v@(T.Local {}) = do
          bs <- Eff.ask

          case Map.lookup v bs of
            Just (bs' :| cs :=> t) | null $ locals t -> do
              Eff.modify $ \sub -> mkSubst (v, t) `compose` sub
              return cs
            Just (bs' :| cs :=> t) -> do
              cs' <- concat <$> forM (locals t) collect
              return $ cs <> cs'


    Typed expr ty -> do
        Typed expr' inferred <- infer expr
        evidence <- Shared.fresh E
          -- | Inferred type is generalized as much as possible and unification expects the LHS to be the subtype
        case expr' of
          -- | When expression is a literal, it doesn't get generalized to preserve the literal type
          Literal _ -> Eff.tell $ CST.Equality evidence (CST.Mono inferred) (CST.Mono ty)
          -- | In any other case, we want to unify by instantiating the inferred type to the src type
          _         -> Eff.tell $ CST.Equality evidence (CST.Mono ty) (CST.Mono inferred)
        return e

    Tuple elems -> do
        elems' <- forM elems $ \e -> infer e
        return $ Typed (Tuple elems') (T.Tuple $ fmap extract elems')

    Record pairs -> do
        pairs' <- forM pairs $ mapM infer
        return $ Typed (Record pairs') (T.Record $ fmap extract <$> pairs')

    List elems -> do
      l <- Eff.gets level
      uvar <- Shared.fresh U
      elems' <-forM elems $ \e -> infer e
      let tys = fmap extract elems'

      forM_ tys $ \t -> do
        ev <- Shared.fresh E
        Eff.tell $ Equality ev (CST.Variable (CST.Level l) uvar) (CST.Mono t)

      return $ Typed (List elems') (T.Applied listConstructor $ T.Var uvar)

    Lambda ps@(param : rest) body -> do

        tvar <- T.Var <$> Shared.fresh U
        let qt = Forall [] (Q.none :=> tvar)
        let scoped = Eff.local (\e -> e { types = Map.insert param qt $ types e })
        scoped $ do
          o@(Typed _ out') <- infer' out
          let ty = tvar `T.Arrow` out'
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
        -- | TODO: By adding row polymorphism, we'd use a different constraint type
        -- | This would allow us to use a `CST.Unification` Item, leading to tracking all unification variables
        -- | Right now, that is not really possible here
        Eff.tell $ CST.Equality evidence (CST.Mono recordTy) (CST.Mono $ T.Record [(field, fieldType)])
        return $ Typed (FnApp dot args) fieldType
    FnApp (Identifier ".") args -> Eff.throwError $ Fail $ "Unrecognised expressions for property access:\n\t" ++ show args

    fapp'@(FnApp fn [arg]) -> do
        pTraceM "\n----------------------------------"
        pTraceM "Inferring FnApp"
        pTraceM $ show fapp'
        out <- T.Var <$> Shared.fresh U

        fn'@(Typed _ fnTy)   <- infer' fn
        pTraceM "Inferred fn:"
        pTraceM $ show fn'

        arg'@(Typed _ argTy) <- infer' arg
        pTraceM "Inferred arg:"
        pTraceM $ show arg'


        --inferred <- generalize $ argTy `T.Arrow` out
        -- | TODO: How can we notify the constraint solver that there's a new unification variable
        -- | which stands for the result of this function application
        --Eff.tell $ CST.Equality evidence (CST.Mono fnTy) (CST.Poly inferred)

        evidence <- Shared.fresh E
        Eff.tell $ CST.Equality evidence (CST.Mono $ argTy `T.Arrow` out) (CST.Mono fnTy)

        return $ Typed (FnApp fn' [arg']) out
    FnApp fn (a : as) -> infer' curried
        where
          partial = FnApp fn [a]
          curried = foldl (\f a -> FnApp f [a]) partial as

    Match scrutinee cases -> do
        scrutinee'@(Typed _ ty) <- infer scrutinee
        cases' <- forM cases (inferCase ty)
        let (tvars, tys) = foldl separate ([], []) cases'
        let ty' = case length tys of
                  0 -> Nothing
                  1 -> Just $ head tys -- | TODO: do we need this anymore? we now collapse unions...
                  _ -> Just $ T.Union tys

        for_ ty' $ \t -> do
          ev <- Shared.fresh E
          Eff.tell $ CST.Equality ev (CST.Mono ty) (CST.Mono t)

        forM_ tvars $ \v -> do
          ev <- Shared.fresh E
          Eff.tell $ CST.Equality ev (CST.Mono v) (maybe (CST.Mono ty) CST.Mono ty')

        let out = T.Union $ fmap extractTy cases'
        return $ Typed (Match scrutinee' cases') out

        where

          inferCase scrutineeType (Case pat expr)  = do
            (patTy, tyvars) <- Eff.runWriter $ Pat.infer pat
            let (pairs, tvars) = unzip $ tyvars ||> fmap (\(id, tvar) -> ((id, Forall [tvar] (Q.none :=> T.Var tvar)), tvar))
            let scoped = Eff.local $ \env -> env { types = Map.fromList pairs <> types env }
            scoped $ do
              (inferred, constraint) <- Eff.listen @Constraint $ infer expr
              ev <- Shared.fresh E
              Eff.tell $ CST.Implication tvars [assume ev scrutineeType patTy] constraint

              return $ TypedCase pat patTy inferred

            where assume ev ty ty' = CST.Assume $ CST.Equality ev (CST.Mono ty) (CST.Mono ty')

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
              ty <- Eff.inject $ E.evaluate typeExp
              let scoped = Eff.local (\e -> e{ types = Map.insert id ty $ types e })
              scoped $ walk (d : processed) rest

            Declaration (Let id (Just typeExp) k expr) -> do
              ty <- Eff.inject $ E.evaluate typeExp
              let scoped = Eff.local (\e -> e{ types = Map.insert id ty $ types e })
              scoped $ do
                (Typed expr' _) <- infer expr
                walk (Declaration (Let id (Just typeExp) k expr') : processed) rest
            Declaration (Let id Nothing k expr) -> do
              l <- Eff.gets level
              uvar <- Shared.fresh U
              let qt = Forall [] (Q.none :=> T.Var uvar)
              let scoped = Eff.local (\e -> e{ types = Map.insert id qt $ types e })
              scoped $ do
                (Typed expr' ty) <- infer expr
                ev <- Shared.fresh E
                Eff.tell $ CST.Equality ev (CST.Variable (CST.Level l) uvar) (CST.Mono ty)
                walk processed rest
            d -> walk (d:processed) rest
    where
      extract (Typed _ ty) = ty


lookup' :: Shared.TypeInference es => String -> Eff es (Qualified Type)
lookup' x = do
  Saga { types } <- Eff.ask

  case Map.lookup x types of
    Just scheme -> walk scheme
    Nothing     -> Eff.throwError $ UnboundVariable x

  where
    walk scheme@(Forall [] qt) = return qt
    walk scheme@(Forall tvars _) = Shared.fresh U >>= walk . instantiate scheme . T.Var




-- run e = Eff.runWriter . Eff.runState initialState . Eff.runFail . Eff.runError . Eff.runWriter . Eff.runReader defaultEnv . infer @Expr $ e
