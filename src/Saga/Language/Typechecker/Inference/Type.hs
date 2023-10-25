
{-# LANGUAGE TypeFamilies #-}


module Saga.Language.Typechecker.Inference.Type where

import           Saga.Language.Core.Expr                       (Declaration (..),
                                                                Expr (..),
                                                                Statement (..))

import qualified Saga.Language.Typechecker.Inference.Inference as I

import qualified Saga.Language.Typechecker.Kind                as K
import           Saga.Language.Typechecker.Kind                (Kind)
import qualified Saga.Language.Typechecker.Type                as T
import           Saga.Language.Typechecker.Type                (Scheme (Forall),
                                                                Type)

import           Control.Monad.RWS
import qualified Data.Map                                      as Map
import qualified Saga.Language.Typechecker.Solver.Constraints  as CST
import           Saga.Language.Typechecker.Solver.Constraints  (Constraint (..))

import           Control.Monad.Except

import           Prelude                                       hiding (lookup)
import           Saga.Language.Typechecker.Errors
import           Saga.Language.Typechecker.Variables           (Classifier,
                                                                Level (..),
                                                                VarType)

import           Control.Applicative                           ((<|>))
import           Data.List                                     hiding (lookup)
import qualified Data.Set                                      as Set
import           Saga.Language.Core.Literals                   (Literal (..))
import           Saga.Language.Typechecker.Environment         (CompilerState (..))
import           Saga.Language.Typechecker.Inference.Inference hiding (lookup)
import           Saga.Language.Typechecker.Protocols           (ProtocolID)
import           Saga.Language.Typechecker.Solver.Substitution (Subst,
                                                                Substitutable (..),
                                                                compose,
                                                                nullSubst)

import qualified Saga.Language.Typechecker.Evaluation          as E
import           Saga.Language.Typechecker.Lib
import qualified Saga.Language.Typechecker.Qualification       as Q
import           Saga.Language.Typechecker.Qualification       (Qualified (..))
import qualified Saga.Language.Typechecker.TypeExpr            as TE
import qualified Saga.Language.Typechecker.Variables           as Var
import           Saga.Utils.Utils


type instance I.Constraint Type = CST.Constraint

type instance VarType Expr I.Evidence       = CST.Evidence
type instance VarType Expr I.Unification    = Var.PolymorphicVar Type
type instance VarType Expr I.Skolem         = Var.PolymorphicVar Type
type instance VarType Expr I.PolyVar        = Var.PolymorphicVar Type
type instance VarType Expr I.Instantiation  = Var.PolymorphicVar Type

-- | Inferring Types of Exprs
instance Inference Expr where

    infer = infer'

    lookup = lookup'
    fresh = fresh'

type TypeInference m = InferM CST.Constraint m

infer' :: TypeInference m => Expr -> m Expr
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
          evidence@(CST.Var p) <- fresh' E
          emit' $ CST.Impl evidence (CST.Mono ty) protocol
          return $ FnApp expr [Identifier p]

    Typed expr ty -> do
        Typed expr' inferred <- infer expr
        evidence <- fresh' E
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
      tvar <- T.Var <$> fresh' U
      elems' <- mapM infer elems
      let tys = fmap extract elems'

      forM_ tys $ \t -> do
        ev <- fresh' E
        emit' $ Equality ev (CST.Mono tvar) (CST.Mono t)

      return $ Typed (List elems') (T.Applied listConstructor tvar)

    Lambda ps@(param : rest) body -> do

        tVar <- fresh' U
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
        fieldType <- T.Var <$> fresh' U
        evidence <- fresh' E
        (Typed _ recordTy) <- infer' recordExpr
        emit' $ CST.Equality evidence (CST.Mono recordTy) (CST.Mono $ T.Record [(field, fieldType)])
        return $ Typed (FnApp dot args) fieldType
    FnApp (Identifier ".") args -> throwError $ Fail $ "Unrecognised expressions for property access:\n\t" ++ show args

    FnApp fn [arg] -> do
        out <- T.Var <$> fresh' U
        fn'@(Typed _ fnTy)   <- infer' fn
        arg'@(Typed _ argTy) <- infer' arg

        inferred <- generalize $ argTy `T.Arrow` out
        evidence <- fresh' E
        emit' $ CST.Equality evidence (CST.Mono fnTy) (CST.Poly inferred)

        return $ Typed (FnApp fn' [arg']) out
    FnApp fn (a : as) -> infer' curried
        where
          partial = FnApp fn [a]
          curried = foldl (\f a -> FnApp f [a]) partial as

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
                walk (Declaration (Let id (Just typeExp) k expr') :processed) rest
            Declaration (Let id Nothing k expr) -> do
              tvar <- fresh' U
              let qt = Forall [] (pure $ T.Var tvar)
              let scoped = local (\e -> e{ types = Map.insert id qt $ types e })
              scoped $ do
                (Typed expr' ty) <- infer expr
                ev <- fresh' E
                emit' $ CST.Equality ev (CST.Mono $ T.Var tvar) (CST.Mono ty)
                walk processed rest
            d -> walk (d:processed) rest
    where
      extract (Typed _ ty) = ty


lookup' :: TypeInference m => String -> m (Qualified Type)
lookup' x = do
  Saga { types } <- ask

  case Map.lookup x types of
    Just scheme -> inst scheme
    Nothing     -> throwError $ UnboundVariable x

  where
    inst scheme@(Forall [] qt) = return qt
    inst scheme@(Forall tvars _) = fresh' U >>= instantiate scheme . T.Var >>= inst


fresh' :: TypeInference m => Tag a -> m (VarType Expr a)
fresh' t = do
  modify $ \s -> s {vars = vars s + 1}
  s <- get
  let count = show ([1 ..] !! vars s)
  return $ case t of
    E -> CST.Var $ "e" ++ count
    U -> Var.Unification ("v" ++ count) (Level $ level s) K.Type
    P -> Var.PolyVar ("p" ++ count) K.Type


instance Instantiate Type  where
  instantiate qt@(Forall [] _) t = return qt
  instantiate (Forall (tvar:tvars) qt) t = return $ Forall tvars qt'
    where
      sub = Map.fromList [(tvar, t)]
      qt' = apply sub qt



instance Generalize Type where
  generalize (T.Tuple tys) = do
    ts <- mapM generalize tys
    let (cs, tvars, ts') = foldl accumulate ([], [], []) ts
    return $ Forall tvars (cs :=> T.Tuple ts')
    where
      accumulate (cs, tvars, ts) (Forall tvars' (cs' :=> t)) = (nub $ cs ++ cs', nub $ tvars ++ tvars', nub $ ts ++ [t])
  generalize (T.Record pairs) = do
    qPairs <- mapM (mapM generalize) pairs
    let (cs, tvars, pairs') = foldl accumulate ([], [], []) qPairs
    return $ Forall tvars (cs :=> T.Record pairs')
    where
      accumulate (cs, tvars, pairs) (key, Forall tvars' (cs' :=> t)) = (nub $ cs ++ cs', nub $ tvars ++ tvars', nub $ pairs ++ [(key, t)])

  generalize (T.Arrow arg out) = do
    Forall tvars (cs :=> arg') <- generalize arg
    return $ Forall tvars (cs :=> arg' `T.Arrow` out)

  generalize (T.Union tys) = do
    tys' <- mapM generalize tys
    let (tvars, cs, qts) = mapM (\(Forall tvars (cs :=> qt)) -> (tvars, cs, qt)) tys'
    return $ Forall (nub tvars) (nub cs :=> T.Union (nub qts))
  generalize ty = case ty of
    T.Singleton lit -> generalize' $ case lit of
      LString _ -> "IsString"
      LBool _   -> "IsBool"
      LInt _    -> "Num"
    T.Data prim _ -> generalize' $ case prim of
      "Int"    -> "Num"
      "String" -> "IsString"
      "Bool"   -> "IsBool"
    _ -> return $ Forall [] ([] :=> ty)

    where
      generalize' :: TypeInference m => ProtocolID -> m (T.Polymorphic Type)
      generalize' protocol = do
        tvar <- fresh' P
        e <- fresh' E
        emit' $ Impl e (CST.Mono $ T.Var tvar) protocol
        return $ Forall [tvar] ([T.Var tvar `Q.Implements` protocol] :=> T.Var tvar)

